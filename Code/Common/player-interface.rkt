#lang racket

(provide
 placements/c
 place/c

 ;; a contract that describes the player class's interface to the administrator 
 player%/c

 ;; a contract that describes the player object's interface to the administrator 
 player/c)
 

;; ---------------------------------------------------------------------------------------------------
(require "board.rkt")
(require "actions.rkt")
  
(define placements/c
  (listof
   (list/c
    string?   ;; who placed a worker 
    in-range? ;; at x 
    in-range? ;; at y on the initial board
    )))

(define place/c (list/c in-range? in-range?))

(define player%/c
  (class/c
   #:opaque
   (init-field (name string?))
   (other
    ;; name of opponent of this player 
    (->m string? any))
   (placement
    ;; compute the placement of this player's next worker, given the placement of other workers
    ;; ASSUME this player knows where it places its players 
    (->m placements/c place/c))
   (take-turn
    ;; compute the next action that this player can take for the given board 
    (->m board? action?))))

(define player/c (instanceof/c player%/c))

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
  
 