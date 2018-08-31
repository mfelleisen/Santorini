#lang racket

;; the contract for a referee observer class and object, respectively 

(provide
 observer%/c
 observer/c)

;; -----------------------------------------------------------------------------

(require "actions.rkt")
(require "board.rkt")

;; -----------------------------------------------------------------------------
(define observer%/c
  (class/c
    [action (->m action? any/c)]
    [board  (->m board? any/c)]
    [report (->m string? any/c)]))

(define observer/c (instanceof/c observer%/c))