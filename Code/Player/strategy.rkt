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
      (cond
        [(empty? list-of-places) (list 0 0)]
        [else (define max-x (apply max (map second list-of-places)))
              (define max-y (apply max (map third  list-of-places)))
              (list (+ 1 max-x) (+ 1 max-y))]))
      

    (define/public (take-turn board player other)
      (define tree    (generate board player other))
      (define actions (tree-actions tree))
      (define fst-act (first actions))
      (if (giving-up? fst-act)
          fst-act
          (or (for/first ((a actions) #:when (winning-move? a)) a)
              (for/first ((a actions) #:when (safe? a tree)) a))))

    ;; Action GameTree -> Boolean 
    (define/private (safe? a gt)
      (define next (step gt a))
      (for/and ((a (tree-actions next)))
        (define mine (step next a))
        ;; there is more than a giving-up action available 
        (cons? (rest (tree-actions mine)))))))

;; ---------------------------------------------------------------------------------------------------
(module+ test (require (submod ".."))


  (define safe (new safe-strategy% [name "x"]))

  (define-board b
    [[1x1 2o1]
     [2x2 1o2]
     [4   4]])

  (define g1 (send safe take-turn b "x" "o"))
  (define g2 (giving-up (worker "x1")))

  (check-equal? (send safe take-turn b "x" "o") (giving-up (worker "x1")))
  (check-equal? (send safe take-turn b "o" "x") (move-build (worker "o1") EAST SOUTH EAST SOUTH))

  (define-board c
    [[1x1 2o1 3]
     [2x2 1o2]
     [4   4]])

  (check-equal? (send safe take-turn c "o" "x") (winning-move (worker "o1") EAST PUT)))