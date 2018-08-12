#lang racket

(provide
 ;; a contract that describes the player class's interface to the administrator 
 player%/c

 ;; a contract that describes the player object's interface to the administrator 
 player/c)
 

;; ---------------------------------------------------------------------------------------------------
(require "board.rkt")
(require "actions.rkt")

(define player%/c
  (class/c
   (init-field name)
   (placement (->m (listof (list/c in-range? in-range?)) (list/c in-range? in-range?)))
   (take-turn (->m board? action?))))

(define player/c (instanceof/c player%/c))