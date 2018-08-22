#lang racket

(provide
 placements/c
 place/c)

;; also provides a json submodule for serializing and deserializing these values

;; -----------------------------------------------------------------------------
(require "board.rkt")

;; -----------------------------------------------------------------------------
(define placements/c
  (listof
   (list/c
    string?   ;; who placed a worker 
    in-range? ;; at x 
    in-range? ;; at y on the initial board
    )))

(define place/c (list/c in-range? in-range?))

;; -----------------------------------------------------------------------------
(module* json #f
  (provide
   placements->jsexpr
   jsexpr->placements

   place->jsexpr
   jsexpr->place)

  (define (placements->jsexpr p)
    p)

  (define (jsexpr->placements j*)
    (map jsexpr->1placement j*))

  (define (jsexpr->1placement j)
    (match j
      [`(,(? string? name) ,(? natural-number/c x) ,(? natural-number/c y)) j]))

  (define (place->jsexpr p) p)

  (define (jsexpr->place j)
    (match j
      [`(,(? natural-number/c x) ,(? natural-number/c y)) j])))

(module+ test
  (require (submod ".." json))
  (require json)
  (require rackunit)

  (check-pred jsexpr? (placements->jsexpr '(("x1" 1 1))))
  (check-pred jsexpr? (place->jsexpr '(1 1)))

  (define ps '(("x1" 1 1) ("y1" 2 2)))
  (check-equal? (jsexpr->placements (placements->jsexpr ps)) ps)

  (define pl '(3 3))
  (check-equal? (jsexpr->place (place->jsexpr pl)) pl))
  
 