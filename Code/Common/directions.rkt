#lang racket

(provide 
 ;; type directions
 EAST WEST NORTH SOUTH PUT
 east-west/c
 north-south/c

 (contract-out
  (e-w->string (-> east-west/c   string?))
  (n-s->string (-> north-south/c string?))))

;; ---------------------------------------------------------------------------------------------------
;; DIRECTIONS
(define NORTH -1)
(define SOUTH +1)
(define PUT    0)
(define EAST  +1)
(define WEST  -1)
(define east-west/c (or/c PUT EAST WEST))
(define north-south/c (or/c NORTH SOUTH PUT))

(define (e-w->string e-w)
  (cond
    [(eq? PUT e-w)  "PUT"]
    [(eq? EAST e-w) "EAST"]
    [(eq? WEST e-w) "WEST"]))

(define (n-s->string e-w)
  (cond
    [(eq? PUT e-w)   "PUT"]
    [(eq? NORTH e-w) "NORTH"]
    [(eq? SOUTH e-w) "SOUTH"]))