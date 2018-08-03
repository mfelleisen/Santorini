#lang racket

;; what knowledge is turned into information here and represented with data:
;; -- a token represents a position on the board 
;; -- a direction tells me where the token goes 
;; -- .. or direction a token eyes for adding a level to a building 

;; ---------------------------------------------------------------------------------------------------

(define DIM 5)
(define in-range? (integer-in 0 DIM))

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
 
 (contract-out

  (token
   (-> string? in-range? in-range? token?))
  
  (token-location
   (-> token? (values in-range? in-range?)))

  (token-name
   (-> token? string?))
  
  (neighbor-location
   (-> token? direction/c direction/c (values in-range? in-range?)))

  (move-token
   (-> token? direction/c direction/c token?))
  
  (at-distinct-places
   ;; are all tokens at distinct places 
   (-> (listof token?) boolean?))

  (all-directions-to-neighbors
   ;; compute all possible directions to a neighboring field from this token
   ;; GUARANTEE (0,0) is not a part of the directions 
   (-> token? (listof (list/c direction/c direction/c))))

  (stay-on-board?
   ;; does this token stay in range if it moves in the specified direction?
   ;; ASSUME token is in range 
   (-> token? direction/c direction/c boolean?))))

;; ---------------------------------------------------------------------------------------------------
(require "../Lib/struct-with.rkt")
(module+ test (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct token (name x y) #:transparent)

(define (token-location t)
  (with token t (values x y)))

(define (move-token t e-w n-s)
  (define-values (x1 y1) (neighbor-location t e-w n-s))
  (token (token-name t) x1 y1))

(define (neighbor-location t e-w n-s)
  (with token t (values (+ x e-w) (+ y n-s))))

(define (all-directions-to-neighbors t)
  (with token t
        (for*/list ((e-w `(,WEST ,PUT ,EAST))
                    (n-s `(,NORTH ,PUT ,SOUTH))
                    (new-e-w (in-value (+ x e-w)))
                    (new-n-s (in-value (+ y n-s)))
                    #:when (and (not (= 0 e-w n-s)) (in-range? new-e-w) (in-range? new-n-s)))
          (list e-w n-s))))

(define (at-distinct-places lot)
  (define L (map (lambda (t) (with token t (list x y))) lot))
  (define N (length L))
  (define S (apply set L))
  (= (set-count S) N))

(define (stay-on-board? t e-w n-s)
  (with token t (and (in-range? (+ x e-w)) (in-range? (+ y n-s)))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  ; (require (submod "..")) ; some of these calls intentionally break contracts 

  (define O (token "christos" 0 0))
  (check-equal? (let-values ([(x y) (neighbor-location O PUT NORTH)]) (list x y)) '(0 -1))
  (check-equal? (let-values ([(x y) (neighbor-location O PUT SOUTH)]) (list x y)) '(0 +1))
  (check-equal? (let-values ([(x y) (neighbor-location O EAST PUT)]) (list x y)) '(+1 0))
  (check-equal? (let-values ([(x y) (neighbor-location O WEST PUT)]) (list x y)) '(-1 0))
  
  (check-true  (at-distinct-places (list (token 'a 1 1) (token 'b 2 2))))
  (check-false (at-distinct-places (list (token 'a 1 1) (token 'b 1 1))))

  (check-equal? (all-directions-to-neighbors O) '((0 1) (1 0) (1 1)))
  (check-equal? (all-directions-to-neighbors (token "mf" 1 0)) '((-1 0) (-1 1) (0 1) (1 0) (1 1)))

  (check-false (stay-on-board? (token "cd" 0 0) PUT NORTH))
  (check-true  (stay-on-board? (token "cd" 0 0) PUT SOUTH))

  (check-equal? (let-values ([(x y) (token-location (token "cd" 0 0))]) (list x y)) '(0 0))

  (check-equal? (move-token (token "cd" 0 0) PUT SOUTH) (token "cd" 0 (+ SOUTH 0))))
