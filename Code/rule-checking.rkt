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
-- if player A can't move or, after moving, can't build up a building
-- if player A's token reaches the third level of a building
|#

;; ---------------------------------------------------------------------------------------------------
(require (only-in "token.rkt" token? direction/c))
(require (only-in "board.rkt" board? on-board? stay-on-board?))

(define simple-checker/c
  (->i ((b board?) (t (b) (and/c token? (on-board? b)))) (r boolean?)))

(define checker/c
  (->i ((b board?) (t (b) (and/c token? (on-board? b))) (e-w direction/c) (n-s direction/c))
       #:pre/name (e-w n-s) "a token can't stay put" (not (and (= e-w PUT) (= n-s PUT)))
       #:pre/name (b t e-w n-s) "stay on board" (stay-on-board? b t e-w n-s) 
       (r boolean?)))

(provide
 (contract-out
  (can-move-and-build? simple-checker/c)
  (check-move checker/c)
  (check-move-end? simple-checker/c)
  (check-build-up checker/c)))

;; ---------------------------------------------------------------------------------------------------
(require (except-in "board.rkt" board? on-board? stay-on-board?))
(require (except-in "token.rkt" token? direction/c))
(require "../Lib/struct-with.rkt")
(module+ test
  (require (submod "board.rkt" test))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (can-move-and-build? b t)
  (with board b
        (with token t
              (define neighbors (all-neighbors x y))
              (for/or ((n neighbors))
                (match-define `(,e-w ,n-s) n)
                (and (check-move b t e-w n-s)
                     (for/or ((n (all-neighbors (+ x e-w) (+ y n-s))))
                       (match-define `(,e-w ,n-s) n)
                       (check-build-up b t e-w n-s)))))))

(define (check-move b t e-w n-s)
  ;; is the new location free?
  ;; is the location below the current one or only 1 step up?
  (with board b
        (with token t
              (and (location-free-of-token? b x y)
                   (check-height-delta? b x y (+ x e-w) (+ y n-s))))))

(define (check-move-end? b t)
  (with board b
        (with token t
              (= (height-of b x y) TOP-FLOOR))))

(define (check-build-up b t e-w n-s)
  (with board b
        (with token t
              (define x1 (+ x e-w))
              (define y1 (+ y n-s))
              (and (location-free-of-token? b x1 y1)
                   (< (height-of b x1 y1) MAX-HEIGHT)))))

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

  (define-board b1
    [[3 2x 1x]
     [3 2o 1o]])
  
  (checker #t check-move b1 ("x" 1 0) WEST PUT)
  (checker #t check-build-up b1 ("x" 1 0) WEST SOUTH)

  (define-board b2
    [[]
     [1b]
     [0 0 0a]
     [0 0 0 0a]
     [0 0 0 0 0b]])
  
  (checker #t check-move      b2 ("b" 0 1) EAST PUT)
  (checker #f check-move-end? b2 ("b" 0 1))
  (checker #t check-build-up  b2 ("b" 0 1) EAST PUT))
