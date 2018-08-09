#lang racket

;; what knowledge is turned into information here and represented with data:
;; -- a worker represents a position on the board 
;; -- a direction tells me where the worker goes 
;; -- .. or direction a worker eyes for adding a level to a building 

;; ---------------------------------------------------------------------------------------------------

(define DIM 5)
(define in-range? (integer-in 0 DIM))

(define NORTH -1)
(define SOUTH +1)
(define PUT    0)
(define EAST  +1)
(define WEST  -1)
(define east-west/c (or/c PUT EAST WEST))
(define north-south/c (or/c NORTH SOUTH PUT))

(provide
 DIM
 ;; type Range = [0,DIM)

 ;; Int -> Boolean
 in-range? 

 NORTH
 SOUTH
 PUT
 EAST
 WEST

 ;; Any -> Boolean
 ;; is this a linear direction 
 east-west/c
 north-south/c

 ;; type Worker = (worker String Range Range)
 worker?
 
 (contract-out

  (worker
   (-> string? in-range? in-range? worker?))
  
  (worker-location
   (-> worker? (values in-range? in-range?)))

  (worker-name
   (-> worker? string?))
  
  (neighbor-location
   (-> worker? east-west/c north-south/c (values in-range? in-range?)))

  (move-worker
   (-> worker? east-west/c north-south/c worker?))
  
  (at-distinct-places
   ;; are all workers at distinct places 
   (-> (listof worker?) boolean?))

  (all-directions-to-neighbors
   ;; compute all possible directions to a neighboring field from this worker
   ;; GUARANTEE (0,0) is not a part of the directions 
   (-> worker? (listof (list/c east-west/c north-south/c))))

  (stay-on-board?
   ;; does this worker stay in range if it moves in the specified direction?
   ;; ASSUME worker is in range 
   (-> worker? east-west/c north-south/c boolean?))))

;; ---------------------------------------------------------------------------------------------------
(require "../Lib/struct-with.rkt")
(module+ test (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct worker (name x y) #:transparent)

(define (worker-location t)
  (with worker t (values x y)))

(define (move-worker t e-w n-s)
  (define-values (x1 y1) (neighbor-location t e-w n-s))
  (worker (worker-name t) x1 y1))

(define (neighbor-location t e-w n-s)
  (with worker t (values (+ x e-w) (+ y n-s))))

(define (all-directions-to-neighbors t)
  (with worker t
        (for*/list ((e-w `(,WEST ,PUT ,EAST))
                    (n-s `(,NORTH ,PUT ,SOUTH))
                    (new-e-w (in-value (+ x e-w)))
                    (new-n-s (in-value (+ y n-s)))
                    #:when (and (not (= 0 e-w n-s)) (in-range? new-e-w) (in-range? new-n-s)))
          (list e-w n-s))))

(define (at-distinct-places lot)
  (define L (map (lambda (t) (with worker t (list x y))) lot))
  (define N (length L))
  (define S (apply set L))
  (= (set-count S) N))

(define (stay-on-board? t e-w n-s)
  (with worker t (and (in-range? (+ x e-w)) (in-range? (+ y n-s)))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  ; (require (submod "..")) ; some of these calls intentionally break contracts 

  (define O (worker "christos" 0 0))
  (check-equal? (let-values ([(x y) (neighbor-location O PUT NORTH)]) (list x y)) '(0 -1))
  (check-equal? (let-values ([(x y) (neighbor-location O PUT SOUTH)]) (list x y)) '(0 +1))
  (check-equal? (let-values ([(x y) (neighbor-location O EAST PUT)]) (list x y)) '(+1 0))
  (check-equal? (let-values ([(x y) (neighbor-location O WEST PUT)]) (list x y)) '(-1 0))
  
  (check-true  (at-distinct-places (list (worker 'a 1 1) (worker 'b 2 2))))
  (check-false (at-distinct-places (list (worker 'a 1 1) (worker 'b 1 1))))

  (check-equal? (all-directions-to-neighbors O) '((0 1) (1 0) (1 1)))
  (check-equal? (all-directions-to-neighbors (worker "mf" 1 0)) '((-1 0) (-1 1) (0 1) (1 0) (1 1)))

  (check-false (stay-on-board? (worker "cd" 0 0) PUT NORTH))
  (check-true  (stay-on-board? (worker "cd" 0 0) PUT SOUTH))

  (check-equal? (let-values ([(x y) (worker-location (worker "cd" 0 0))]) (list x y)) '(0 0))

  (check-equal? (move-worker (worker "cd" 0 0) PUT SOUTH) (worker "cd" 0 (+ SOUTH 0))))
