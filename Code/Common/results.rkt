#lang racket

;; This file describe the results of a tournament. 

(provide
 result/c
 result*/c)

;; -----------------------------------------------------------------------------
(module+ test
  (require rackunit))

;; -----------------------------------------------------------------------------
(define result/c  (list/c string? #;=winner string? #;=loser))
(define result*/c (listof result/c))

;; -----------------------------------------------------------------------------
(module json racket
  (provide
   results->jsexpr
   jsexpr->results)

  (define results->jsexpr values)
    

  (define (jsexpr->results j)
    (match j
      [`((,(? string?) ,(? string?)) ...) j]
      [else #false])))

(module+ test
  (require (submod ".." json))

  (check-equal? (jsexpr->results '(("one" "two"))) '(("one" "two")))
  (check-false (jsexpr->results "oneone")))