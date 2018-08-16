#lang racket

;; a strategy that picks a move so that the opponent cannot block you from an action for the next move

(require "strategy-interface.rkt")

(provide
 (contract-out
  (safe-strategy% strategy%/c)))

;; ---------------------------------------------------------------------------------------------------
(require "move-generating.rkt")
(require "../Common/board.rkt")
(require "../Common/directions.rkt")
(require "../Common/worker.rkt")
(require "../Common/actions.rkt")
(module+ test
  (require (submod "../Common/board.rkt" test))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define safe-strategy%
  (class object% (init-field player other)

    (super-new)
    
    (define/public (initialization list-of-places)
      (initialization1 list-of-places))

    ;; [Listof [List String N N]] -> [List N N]
    ;; maximize distance from other players 
    (define/private (initialization1 lop)
      (cond
        [(empty? lop) (list 0 0)]
        [else
         (define others (filter (compose (curry string=? other) first) lop))
         (define free-places (non-occupied-places (map rest lop)))
         (define with-distances
           (for*/list ((f free-places) (o (map rest others)))
             (list f (distance f o))))
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
  
  (define xsafe (new safe-strategy% [player "x"] [other "o"]))
  (define osafe (new safe-strategy% [player "o"] [other "x"]))

  (check-pred cons? (send xsafe initialization '()) "just to make sure that the mechanics work out")
  (check-equal? (send xsafe initialization '()) (list 0 0))
  (check-equal? (send xsafe initialization (list (list "o" 0 0))) (list 5 5))

  (define o1 (worker "o1"))
  (define gu-mb-board
    (cboard 
     [[1x1 2o1]
      [2x2 1o2]
      [4   4]]))
  
  (check-equal? (send xsafe take-turn gu-mb-board) (giving-up "x"))
  (check-equal? (send osafe take-turn gu-mb-board) (move-build o1 EAST SOUTH EAST SOUTH))

  (define winning-board
    (cboard
     [[1x1 2o1 3]
      [2x2 1o2]
      [4   4]]))

  (check-equal? (send osafe take-turn winning-board) (winning-move o1 EAST PUT))

  (define gu-2-down-board
    (cboard 
     [[1x1 2o1 3]
      [2x2 1o2]
      [2   4]
      [4   4]]))

  (check-equal? (send xsafe take-turn gu-2-down-board) (giving-up "x")))
