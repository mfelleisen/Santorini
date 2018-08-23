#lang racket

;; a strategy that picks a move so that the opponent cannot block you from an action for the next move

(require "strategy-interface.rkt")

(provide
 (contract-out
  (strategy% strategy%/c)))

;; ---------------------------------------------------------------------------------------------------
(require "move-generating.rkt")
(require "../Common/player-interface.rkt")
(module+ test
  (require (submod "../Common/board.rkt" test))
  (require "../Lib/with-output-to-dev-null.rkt")
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define strategy%
  (class object% (init-field player other)

    (super-new)
    
    (define/public (initialization list-of-places)
      (init-to-as-far-away-as-possible list-of-places))

    ;; [Listof [List String N N]] -> [List N N]
    ;; maximize distance from other players 
    (define/private (init-to-as-far-away-as-possible lop)
      (cond
        [(empty? lop) (list 0 0)]
        [else
         (define others (map rest (filter (compose (curry string=? other) first) lop)))
         (define free-places (non-occupied-places (map rest lop)))
         (define with-distances
           (for/list ((f free-places))
             (list f (for/sum ((o others)) (distance f o)))))
         (first (argmax second with-distances))]))
    
    (define/public (take-turn board (n 2))
      (define tree (generate board player other))
      ;; GameTree N -> Action
      ;; find first action that is a winner or guarantees n move-and-build turns
      ;; if all else fails, give up 
      (define (find-a-good-action tree n)
        (define actions (tree-actions tree))
        (or (for/first ((a actions) #:when (winning-move? a)) a)       
            (for/first ((a actions) #:when (or (<= n 1) (safe? a tree (sub1 n)))) a)
            (giving-up player)))

      ;; Action GameTree N -> Boolean 
      (define (safe? a gt n)
        (define next (step gt a))
        (for/and ((a (tree-actions next)))
          (define mine (step next a))
          (define acts (tree-actions mine))
          (and
           ;; there is more than a giving-up action available 
           (cons? (rest acts))
           (not (giving-up? (find-a-good-action mine (sub1 n)))))))
      ;; -- IN -- 
      (find-a-good-action tree n))))

(define (distance p q)
  (sqrt (apply + (map sqr (map - p q)))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  
  (check-equal? (distance (list 3 4) (list 0 0)) 5)
  (check-equal? (distance (list 12 5) (list 9 1)) 5)
  
  (define (make-safe x o) (new strategy% [player x] [other o]))

  ;; -------------------------------------------------------------------------------------------------
  ;; initialization 
  (check-pred cons? (send (make-safe "x" "o") initialization '()) "the mechanics work out")
  
  (define x-o-safe (make-safe "x" "o"))
  (check-equal? (send x-o-safe initialization '()) (list 0 0))
  (check-equal? (send x-o-safe initialization `(("x" 1 1)("o" 0 0))) (list 5 5))

  (define x-o-snd-call-safe (make-safe "x" "o"))
  (check-equal? (begin
                  (send x-o-snd-call-safe initialization '())
                  (send x-o-snd-call-safe initialization `(("o" 0 0)("x" 5 5))))
                (list 4 5))

  (define o-x--snd-call-safe (make-safe "o" "x"))
  (check-equal? (begin
                  (send o-x--snd-call-safe initialization '())
                  (send o-x--snd-call-safe initialization `(("x" 4 5)("o" 0 0)("x" 5 5))))
                (list 1 0))

  (define-syntax-rule
    (check-protocol lop1 lop2 ... msg)
    (check-exn exn:fail:contract?
               (lambda ()
                 (with-output-to-dev-null #:error-port #true
                     (lambda ()
                       (define x-o (make-safe "x" "o"))
                       (send x-o initialization 'lop1)
                       (send x-o initialization 'lop2) ...)))
               msg))

  (check-protocol (("x" 1 1)) "first call, but already placed a token")
  (check-protocol (("o" 1 1)) (("o" 1 1)) "second call, 'I' have not placed a token yet")
  (check-protocol (("o" 1 1)) (("x" 1 1)) "second call, 'other' has not placed a token yet")

  ;; -------------------------------------------------------------------------------------------------
  ;; take turn 
  (define o1 (worker "o1"))
  (define gu-mb-board
    (cboard 
     [[1x1 2o1]
      [2x2 1o2]
      [4   4]]))

  (define x-o-take-turn (make-safe "x" "o"))
  (define o-x-take-turn (make-safe "o" "x"))

  (check-equal? (send x-o-take-turn take-turn gu-mb-board) (giving-up "x"))
  (check-equal? (send o-x-take-turn take-turn gu-mb-board) (move-build o1 EAST SOUTH EAST SOUTH))

  (define winning-board
    (cboard
     [[1x1 2o1 3]
      [2x2 1o2]
      [4   4]]))

  (check-equal? (send o-x-take-turn take-turn winning-board) (winning-move o1 EAST PUT))

  (define gu-2-down-board
    (cboard 
     [[1x1 2o1 3]
      [2x2 1o2]
      [2   4]
      [4   4]]))

  (check-equal? (send x-o-take-turn take-turn gu-2-down-board) (giving-up "x")))
