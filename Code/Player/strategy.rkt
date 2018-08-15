#lang racket

;; a strategy that picks a move so that the opponent cannot block you from an action for the next move

(require "strategy-interface.rkt")

(provide
 (contract-out
  (safe-strategy% strategy%/c)))

;; ---------------------------------------------------------------------------------------------------
(require "move-generating.rkt")
(require "../Common/directions.rkt")
(require "../Common/worker.rkt")
(require "../Common/actions.rkt")
(module+ test
  (require (submod "../Common/board.rkt" test))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define safe-strategy%
  (class object% (init-field name)

    (super-new)

    (define/public (initialization list-of-places)
      (initialization1 list-of-places))
      
    (define/public (take-turn board player other)
      (define tree    (generate board player other))
      (define actions (tree-actions tree))
      (define fst-act (first actions))
      (cond
        [(giving-up? fst-act) fst-act]
        [(for/first ((a actions) #:when (winning-move? a)) a) => values]
        [(for/first ((a actions) #:when (safe? a tree)) a) => values]
        [else (giving-up name)]))

    ;; Action GameTree -> Boolean 
    (define/private (safe? a gt)
      (define next (step gt a))
      (for/and ((a (tree-actions next)))
        (define mine (step next a))
        ;; there is more than a giving-up action available 
        (cons? (rest (tree-actions mine)))))))

(define (initialization1 list-of-places)
  (cond
    [(empty? list-of-places) (list 0 0)]
    [else (define max-x (apply max (map second list-of-places)))
          (define max-y (apply max (map third  list-of-places)))
          (list (+ 1 max-x) (+ 1 max-y))]))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))

  (check-equal? (initialization1 `()) (list 0 0))
  (check-equal? (initialization1 `(("x" 0 0))) (list 1 1))
  
  (define safe (new safe-strategy% [name "x"]))

  (check-pred cons? (send safe initialization '()) "just to make sure that the mechanics work out")

  (define o1 (worker "o1"))
  (define-board gu-mb-board
    [[1x1 2o1]
     [2x2 1o2]
     [4   4]])
  
  (check-equal? (send safe take-turn gu-mb-board "x" "o") (giving-up "x"))
  (check-equal? (send safe take-turn gu-mb-board "o" "x") (move-build o1 EAST SOUTH EAST SOUTH))

  (define-board winning-board
    [[1x1 2o1 3]
     [2x2 1o2]
     [4   4]])

  (check-equal? (send safe take-turn winning-board "o" "x") (winning-move o1 EAST PUT))

  (define-board gu-2-down-board
    [[1x1 2o1 3]
     [2x2 1o2]
     [2   4]
     [4   4]])

  (check-equal? (send safe take-turn gu-2-down-board "x" "o") (giving-up "x"))
  

  )


