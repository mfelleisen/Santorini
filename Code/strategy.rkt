#lang racket

;; strategy:
;;  pick an action so that the opponent cannot block you from an action for the next move

(require "move-generating.rkt")

(require "../Lib/struct-with.rkt")
(module+ test
  (require (submod "board.rkt" test))
  (require rackunit))

(define (strategy board player other)
  (define gt (generate board player other))
  (for/first ((a (tree-actions gt)) #:when (safe? a gt)) a))

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

  (strategy b "x" "o")
  
  (strategy b "o" "x"))

 