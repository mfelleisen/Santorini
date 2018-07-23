#lang racket

;; what knowledge is turned into information here and represented with data:
;; -- a token represents a position on the board 
;; -- a direction tells me where the token goes 
;; -- .. or direction a token eyes for adding a level to a building 

;; ---------------------------------------------------------------------------------------------------

(define DIM 5)

(define NORTH -1)
(define SOUTH +1)
(define PUT    0)
(define EAST  +1)
(define WEST  -1)
(define direction/c (or/c NORTH SOUTH PUT EAST WEST))

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
 direction/c

 ;; type Token = (token String Range Range)
 token?

 position-of

 #;
 (token
   ;; create a token for a specific place and of a specific name 
   (-> string? in-range? in-range? token?))
 token
 
 (contract-out
  
  (token-location
   (-> token? (values in-range? in-range?)))
  
  (move-token
   (-> token? direction/c direction/c token?))
  
  (at-distinct-places
   ;; are all tokens at distinct places 
   (-> (listof token?) boolean?))

  (pick-all-neighbors
   ;; compute all possible directions to a neighboring field from here
   ;; ASSUME token is on board? 
   (-> token? (listof (list/c in-range? in-range?))))))

;; -----------------------------------------------------------------------------
(require "../Lib/struct-with.rkt")
(module+ test (require rackunit))

;; -----------------------------------------------------------------------------
(define (in-range? i)
  (<= 0 i DIM))

(struct token (color x y) #:transparent)

(define (token-location t)
  (with token t (values x y)))

(define (move-token t e-w n-s)
  (define-values (x1 y1) (position-of t e-w n-s))
  (token x1 y1))

(define (position-of t e-w n-s)
  (with token t (values (+ x e-w) (+ y n-s))))

(define (pick-all-neighbors t)
  (with token t
    (for*/list ((e-w `(,WEST ,PUT ,EAST))
                (n-s `(,NORTH ,PUT ,SOUTH))
                #:when (and (in-range? (+ x e-w)) (in-range? (+ y n-s))))
      (list e-w n-s))))

(define (at-distinct-places lot)
  (define L (map (lambda (t) (with token t (list x y))) lot))
  (define N (length L))
  (define S (apply set L))
  (= (set-count S) N))

;; -----------------------------------------------------------------------------
(module+ test
  (define O (token "christos" 0 0))
  (check-equal? (let-values ([(x y) (position-of O PUT NORTH)]) (list x y)) '(0 -1))
  (check-equal? (let-values ([(x y) (position-of O PUT SOUTH)]) (list x y)) '(0 +1))
  (check-equal? (let-values ([(x y) (position-of O EAST PUT)]) (list x y)) '(+1 0))
  (check-equal? (let-values ([(x y) (position-of O WEST PUT)]) (list x y)) '(-1 0))

  (check-true  (at-distinct-places (list (token 'a 1 1) (token 'b 2 2))))
  (check-false (at-distinct-places (list (token 'a 1 1) (token 'b 1 1))))) 
