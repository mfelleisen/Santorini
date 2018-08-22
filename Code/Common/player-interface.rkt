#lang racket

(provide
 ;; a contract that describes the player class's interface to the administrator 
 player%/c

 ;; a contract that describes the player object's interface to the administrator 
 player/c

 ;; the rest of the interface 
 (all-from-out "actions.rkt")
 (all-from-out "placements.rkt")
 (all-from-out "board.rkt"))

;; ---------------------------------------------------------------------------------------------------
(require "board.rkt")
(require "actions.rkt")
(require "placements.rkt")

;; ---------------------------------------------------------------------------------------------------
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