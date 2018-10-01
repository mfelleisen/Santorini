#lang racket

#| The Rules
;; ---------------------------------------------------------------------------------------------------

If it is player's P turn, P must (1) move one worker and (2) build up one building after the move.

A worker can move to a neighboring place if

-- there is no other player on that field,
-- he is "jumping" down from a building (of arbitrary height), or
-- the building on this place is only one step taller than the one he is on and not capped.

A player can add a level to a neighboring field if the building isn't already capped. 

The game ends
-- if player P can't move or, after moving, can't build up a building
-- if player P's worker reaches the third level of a building
|#

;; ---------------------------------------------------------------------------------------------------
(provide
 (contract-out
  (can-move-and-build?
   ;; can the given worker move on this board and then build up a house on this board? 
   (->i ((b board?) (t (b) (and/c worker? (on-board? b)))) (r boolean?)))
  
  (check-move
   ;; can the given worker move in the specified direction on this board? 
   (->i ((b board?) (t (b) (and/c worker? (on-board? b))) (e-w east-west/c) (n-s north-south/c))
        (r boolean?)))
  
  (check-build-up
   ;; can the worker move to (e-w,n-s) and then build on (b-e-w,b-n-s) on this board?
   ;; ASSUME check-move has been called to make sure the worker move is okay 
   (->i ((b board?)
         (t (b) (and/c worker? (on-board? b)))
         (e-w east-west/c) (n-s north-south/c)
         (b-e-w east-west/c) (b-n-s north-south/c))
        (r boolean?)))))
 
;; ---------------------------------------------------------------------------------------------------
(require "workers.rkt")
(require "directions.rkt")
(require "buildings.rkt")
(require "board.rkt")

(module+ test
  (require (submod "../Common/board.rkt" test-support))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (can-move-and-build? b t)
  (for/or ((n (all-directions-to-neighbors b t)))
    (match-define `(,e-w ,n-s) n)
    (and (check-move b t e-w n-s)
         (for/or ((n (all-directions-to-neighbors (move b t e-w n-s) t)))
           (match-define `(,b-e-w ,b-n-s) n)
           (check-build-up b t e-w n-s b-e-w b-n-s)))))

(define (check-move b t e-w n-s)
  (check-it b t e-w n-s check-height-delta?))

(define (check-build-up b t e-w n-s b-e-w b-n-s)
  (and (check-move b t e-w n-s)
       (check-it (move b t e-w n-s) t b-e-w b-n-s short-building)))

#; [Board Worker E-W-Dir N-S-Dir [Board Worker E-W N-S -> Boolean] -> Boolean]
(define (check-it b t e-w n-s f)
  (and (not (and (= e-w PUT) (= n-s PUT)))
       (stay-on-board? b t e-w n-s)
       (location-free-of-worker? b t e-w n-s)
       (f b t e-w n-s)))

;; Board Worker E-W-Dir N-S-Dir -> Boolean
;; is the up-delta <= 1 or is it going down?
(define (check-height-delta? b t e-w n-s)
  (define z0 (height-of b t))
  (define z1 (height-of b t e-w n-s))
  (define delta (- z1 z0))
  (or (<= delta 0) (= delta 1)))

;; Board Worker E-W N-S -> Boolean
;; is the indicated building short enough to build up? 
(define (short-building new-board t b-e-w b-n-s)
  (< (height-of new-board t b-e-w b-n-s) MAX-HEIGHT))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  
  (define-syntax-rule
    (checker r f b (c) arg ...)
    (check-equal? (f b (worker c) arg ...) r))

  (define-syntax-rule
    (check-fail f b (c) arg ...)
    (check-exn exn:fail:contract? (lambda () (f b (worker c) arg ...))))

  (define (board-move tt)
    (cboard 
     [[3 ,tt 1x2]
      [4 2o1 1o2]]))

  (define b1-before (board-move '2x1))
  (define b1-after  (board-move '3x1))
  
  (define b2
    (cboard
     [[]
      [1b1]
      [0 0 0a1]
      [0 0 0 0a2]
      [0 0 0 1 0b2]]))
  
  (checker #t check-move b1-before ("x1") WEST PUT)
  (checker #t check-move b2 ("b1") EAST PUT)
  (checker #t check-move b2 ("b2") WEST PUT)
  (checker #t check-move b2 ("b2") PUT NORTH)
  
  (checker #t check-build-up b2 ("b1") EAST SOUTH WEST PUT)
  (checker #f check-build-up b1-before ("x1") WEST PUT PUT SOUTH)
  (checker #t check-build-up b1-before ("o1") PUT SOUTH PUT SOUTH)
  (checker #t can-move-and-build? b1-before ("x2"))
  (checker #f can-move-and-build? (board-move '0x1) ("x1"))

  (check-fail check-move (worker "b1") ("b1") WEST PUT)

  (define b-bug-in-move-generating
    (cboard [[2o1 1x1]  ;; (2,0) -> 1.1 1.2 1.3 3.0
             [2x2 1o2]  ;; (2,1) -> 3.2 2.2 3.2 2.0 3.0
             [4   4 ]]))
  (checker #f check-build-up b-bug-in-move-generating ("o2") EAST PUT WEST NORTH)
  (checker #f check-build-up b-bug-in-move-generating ("o2") EAST NORTH WEST PUT))
