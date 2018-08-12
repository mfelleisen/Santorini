#lang racket

;; BUILDINGS

(provide
 ;; type Building = (building Range Range N)
 MAX-HEIGHT ; a buidling is called 'capped' if its MAX-HEIGHT stories tall. 
 TOP-FLOOR  ; this is the victory story

 building
 building-height)

;; -----------------------------------------------------------------------------
(struct building (height) #:transparent)
(define TOP-FLOOR  3)
(define MAX-HEIGHT 4)
