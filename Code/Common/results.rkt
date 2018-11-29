#lang racket

;; This file describe the results of a tournament. 

(provide
 IRREGULAR
 result/c
 result*/c)

;; -----------------------------------------------------------------------------
(module+ test
  (require rackunit))

;; -----------------------------------------------------------------------------
(define result/c
  (or/c (list/c string? #;=winner string? #;=loser)
        (list/c string? #;=winner string? #;=loser IRREGULAR)))
(define result*/c (listof result/c))

;; -----------------------------------------------------------------------------
(module json racket
  (provide
   IRREGULAR
   results->jsexpr
   jsexpr->results)
  
  (define IRREGULAR "irregular")
  
  (define results->jsexpr values)

  (define (jsexpr->results j)
    (match j
      [(list (or (list (? string?) (? string?))
                 (list (? string?) (? string?) (? (curry string=? IRREGULAR))))
             ...)
       j]
      [else #false])))

(require 'json)

(module+ test
  (require (submod ".." json))

  (define plain '(("one" "two")))
  (define irreg `(("one" "two" ,IRREGULAR)))
  (check-equal? (jsexpr->results plain) plain)
  (check-equal? (jsexpr->results irreg) irreg)
  (check-false (jsexpr->results "oneone")))