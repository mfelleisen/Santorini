#lang racket

(require "../Lib/require.rkt")
(require+ "../Common/board.rkt" board? on?)

(define (name/c b) (and/c string? (on? b)))

(provide
 (contract-out 
  (safe-strategy
   ;;  pick the first action so that the opponent cannot block you from an action for the next move
   (->i ((b board?) (player (b) (name/c b)) (other (b) (name/c b))) (r (or/c #f action?))))))

;; ---------------------------------------------------------------------------------------------------
(require "move-generating.rkt")
(require- "../Common/board.rkt" on? board?)
(module+ test
  (require (submod "../Common/board.rkt" test))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (safe-strategy board player other)
  (define gt (generate board player other))
  (for/first ((a (tree-actions gt)) #:when (safe? a gt))
    a))

;; Action GameTree -> Boolean 
(define (safe? a gt)
  (define next (step gt a))
  (for/and ((a (tree-actions next)))
    (define mine (step next a))
    (cons? (tree-actions mine))))

(module+ test
  (require (submod ".."))

  (define-board b
    [[1x 2o]
     [2x 1o]
     [4  4]])

  (check-false (safe-strategy b "x" "o"))
  (check-equal? (safe-strategy b "o" "x") (action (token "o" 1 0) 1 1 1 1)))
