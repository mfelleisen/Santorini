#lang racket

;; BUILDINGS

(provide
 ;; type Building = (building Range Range N)
 MAX-HEIGHT ; a buidling is called 'capped' if its MAX-HEIGHT stories tall. 
 TOP-FLOOR  ; this is the victory story

 building
 building-height

 same-building)

;; -----------------------------------------------------------------------------
(struct building (x y height) #:transparent)
(define TOP-FLOOR  3)
(define MAX-HEIGHT 4)

(define (same-building b1)
  (match-define (building x1 y1 z1) b1)
  (lambda (b2)
    (match-define (building x2 y2 z2) b2)
    (and (= x1 x2) (= y1 y2))))