#lang racket

(provide 
 ;; type directions
 EAST WEST NORTH SOUTH PUT
 east-west/c
 north-south/c)

;; ---------------------------------------------------------------------------------------------------
;; DIRECTIONS
(define NORTH -1)
(define SOUTH +1)
(define PUT    0)
(define EAST  +1)
(define WEST  -1)
(define east-west/c (or/c PUT EAST WEST))
(define north-south/c (or/c NORTH SOUTH PUT))