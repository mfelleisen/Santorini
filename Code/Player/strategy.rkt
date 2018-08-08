#lang racket

(require "../Lib/require.rkt")
(require+ "../Common/board.rkt" board? on?)
(require+ "../Common/actions.rkt" action?)

(define (name/c b) (and/c string? (on? b)))

(provide
 (contract-out 
  (safe-strategy
   ;;  pick the first action so that the opponent cannot block you from an action for the next move
   (->i ((b board?) (player (b) (name/c b)) (other (b) (name/c b))) (r (or/c #f action?))))))

;; ---------------------------------------------------------------------------------------------------
(require "move-generating.rkt")
(require- "../Common/board.rkt" on? board?)
(require- "../Common/actions.rkt" action?)
(module+ test
  (require (submod "../Common/board.rkt" test))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (safe-strategy board player other)
  (define tree    (generate board player other))
  (define actions (tree-actions tree))
  (define fst-act (first actions))
  (if (giving-up? fst-act)
      fst-act
      (or (for/first ((a actions) #:when (winning-move? a)) a)
          (for/first ((a actions) #:when (safe? a tree)) a))))

;; Action GameTree -> Boolean 
(define (safe? a gt)
  (define next (step gt a))
  (for/and ((a (tree-actions next)))
    (define mine (step next a))
    (cons? (tree-actions mine))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))

  (define-board b
    [[1x 2o]
     [2x 1o]
     [4  4]])

  (check-equal? (safe-strategy b "x" "o") (giving-up))
  (check-equal? (safe-strategy b "o" "x") (move-build (token "o" 1 0) EAST SOUTH EAST SOUTH))

  (define-board c
    [[1x 2o 3]
     [2x 1o]
     [4  4]])

  (check-equal? (safe-strategy c "o" "x") (winning-move (token "o" 1 0) EAST PUT)))