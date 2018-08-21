#lang racket

(provide 
 ;; type directions
 direction/c ;; is one of: 
 east-west/c
 north-south/c

 (contract-out
  (EAST  east-west/c)
  (WEST  east-west/c)
  (NORTH north-south/c)
  (SOUTH north-south/c)
  (PUT   direction/c)))

(module+ json
  (provide 
   (contract-out
    (e-w->string (-> east-west/c   string?))
    (n-s->string (-> north-south/c string?))
    (string->e-w (-> string?   east-west/c))
    (string->n-s (-> string? north-south/c)))))

;; ---------------------------------------------------------------------------------------------------
;; DIRECTIONS
(define NORTH -1)
(define SOUTH +1)
(define PUT    0)
(define EAST  +1)
(define WEST  -1)
(define east-west/c (or/c PUT EAST WEST))
(define north-south/c (or/c NORTH SOUTH PUT))
(define direction/c (or/c east-west/c north-south/c))

(module+ json 
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

  (define (string->e-w e-w)
    (case e-w
      [("PUT")  PUT]
      [("EAST") EAST]
      [("WEST") WEST]
      [else (error string->e-w "EAST or WEST or PUT expected, given ~e" e-w)]))

  (define (string->n-s e-w)
    (case e-w
      [("PUT")   PUT]
      [("NORTH") NORTH]
      [("SOUTH") SOUTH]
      [else (error string->e-w "NORTH or SOUTH or PUT expected, given ~e" e-w)])))