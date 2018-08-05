#lang racket

#| The Rules
;; ---------------------------------------------------------------------------------------------------

If it is player's P turn, P must (1) move one token and (2) build up one building after the move.

A token can move to a neighboring place if

-- there is no other player on that field,
-- he is "jumping" down from a building (of arbitrary height), or
-- the building on this place is only one step taller than the one he is on and not capped.

A player can add a level to a neighboring field if the building isn't already capped. 

The game ends
-- if player P can't move or, after moving, can't build up a building
-- if player P's token reaches the third level of a building
|#

;; ---------------------------------------------------------------------------------------------------
(require (only-in "token.rkt" token? direction/c stay-on-board?))
(require (only-in "board.rkt" board? on-board?))

(define simple-checker/c
  (->i ((b board?) (t (b) (and/c token? (on-board? b)))) (r boolean?)))

(define checker/c
  (->i ((b board?) (t (b) (and/c token? (on-board? b))) (e-w direction/c) (n-s direction/c))
       #:pre/name (e-w n-s) "a token can't stay put" (not (and (= e-w PUT) (= n-s PUT)))
       #:pre/name (b t e-w n-s) "stay on board" (stay-on-board? t e-w n-s) 
       (r boolean?)))

(provide
 (contract-out
  ;; can the given token move on this board and then build up a house on this board? 
  (can-move-and-build? simple-checker/c)

  ;; can the given token move in the specified direction on this board? 
  (check-move checker/c)

  ;; did the move of the token end the game on this board? 
  (check-move-end? simple-checker/c)

  ;; can the token build in the specified direction on this board? 
  (check-build-up checker/c)))

;; ---------------------------------------------------------------------------------------------------
(require (except-in "board.rkt" board? on-board?))
(require (except-in "token.rkt" token? direction/c stay-on-board?))
(module+ test
  (require (submod "board.rkt" test))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (can-move-and-build? b t)
  (for/or ((n (all-directions-to-neighbors t)))
    (match-define `(,e-w ,n-s) n)
    (and (check-move b t e-w n-s)
         (let* ([new-t   (move-token t e-w n-s)]
                [b-moved (move b t e-w n-s)])
           (for/or ((n (all-directions-to-neighbors new-t)))
             (match-define `(,e-w ,n-s) n)
             (check-build-up b-moved new-t e-w n-s))))))

(define (check-move b t e-w n-s)
  (define-values (x y) (token-location t))
  (define-values (x1 y1) (neighbor-location t e-w n-s))
  (and (location-free-of-token? b x1 y1) (check-height-delta? b x y x1 y1)))

(define (check-build-up b t e-w n-s)
  (define-values (x y) (token-location t))
  (define-values (x1 y1) (neighbor-location t e-w n-s))
  (and (location-free-of-token? b x1 y1) (< (height-of b x1 y1) MAX-HEIGHT)))

(define (check-move-end? b t)
  (define-values (x y) (token-location t))
  (= (height-of b x y) TOP-FLOOR))

;; Board Range Range Range Range -> Board
;; is the up-delta <= 1 or is it going down?
(define (check-height-delta? b x0 y0 x1 y1)
  (define z0 (height-of b x0 y0))
  (define z1 (height-of b x1 y1))
  (define delta (- z1 z0))
  (or (<= delta 0) (= delta 1)))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  
  (define-syntax-rule
    (checker r f b (c tx ty) arg ...)
    (check-equal? (f b (token c tx ty) arg ...) r))

  (define-syntax-rule
    (check-fail f b (c tx ty) arg ...)
    (check-exn exn:fail:contract? (lambda () (f b (token c tx ty) arg ...))))

  (define (board-move tt)
    (define-board b1
      [[3 ,tt 1x]
       [3 2o 1o]])
    b1)

  (define b1-before (board-move (list 2 "x")))
  (define b1-after  (board-move (list 3 "x")))
  
  (define-board b2
    [[]
     [1b]
     [0 0 0a]
     [0 0 0 0a]
     [0 0 0 1 0b]])
  
  (checker #t check-move b1-before ("x" 1 0) WEST PUT)
  (checker #t check-move b2 ("b" 0 1) EAST PUT)
  (checker #t check-move b2 ("b" 4 4) WEST PUT)
  (checker #t check-move b2 ("b" 4 4) PUT NORTH)
  
  (checker #t check-move-end? b1-after ("x" 1 0))
  (checker #f check-move-end? b2 ("b" 0 1))
  
  (checker #t check-build-up b1-before ("x" 1 0) WEST SOUTH)
  (checker #f check-build-up b1-before ("x" 1 0) PUT  SOUTH)
  (checker #t check-build-up b2        ("b" 0 1) EAST PUT)

  (checker #t can-move-and-build? b1-before ("x" 1 0))
  (checker #f can-move-and-build? (board-move (list 0 "x")) ("x" 1 0))
  
  (check-fail check-move (move b2 (token "b" 0 1) PUT SOUTH) ("b" 0 1) EAST PUT))
