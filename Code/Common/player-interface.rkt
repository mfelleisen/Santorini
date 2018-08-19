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
   (init-field (name string?) strategy%)
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
