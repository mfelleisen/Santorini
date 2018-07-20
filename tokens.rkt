#lang racket

;; what knowledge is turned into information here and represented with data:
;; -- a token represents a position on the board 
;; -- a direction tells me where the token goes 
;; -- .. or direction a token eyes for adding a level to a building 

;; ---------------------------------------------------------------------------------------------------

(define DIM 5)
(define NORTH (gensym))
(define SOUTH (gensym))
(define EAST  (gensym))
(define WEST  (gensym))

(provide
 DIM
 ;; type Range = [0,DIM)

 NORTH
 SOUTH
 EAST
 WEST 
 ;; type Direction =  NORTH | SOUTH | EAST | WEST 

 ;; type Token = (token Int Int)
 
 ;; Range Range -> Token
 token

 ;; Token -> (values Range Range)
 token-location

 ;; Token Direction -> Token 
 move-token

 ;; Token Direction -> (values Int Int)
 position-of)

;; -----------------------------------------------------------------------------
(require "Lib/struct-with.rkt")
(module+ test (require rackunit))

;; -----------------------------------------------------------------------------
(struct-with token (x y) #:transparent)

(define (token-location t)
  (with-token t (values x y)))

(define (move-token t d)
  (define-values (x1 y1) (position-of t d))
  (token x1 y1))
      
(define (position-of t d)
  (with-token t
    (cond
      [(eq? d NORTH) (values x        (+ y +1))]
      [(eq? d SOUTH) (values x        (+ y -1))]
      [(eq? d EAST)  (values (+ x +1) y)]
      [(eq? d WEST)  (values (+ x -1) y)])))

;; -----------------------------------------------------------------------------
(module+ test
  (define O (token 0 0))
  (check-equal? (let-values ([(x y) (position-of O NORTH)]) (list x y)) '(0 +1))
  (check-equal? (let-values ([(x y) (position-of O SOUTH)]) (list x y)) '(0 -1))
  (check-equal? (let-values ([(x y) (position-of O EAST)]) (list x y)) '(+1 0))
  (check-equal? (let-values ([(x y) (position-of O WEST)]) (list x y)) '(-1 0)))
                  