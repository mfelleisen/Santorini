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
(require "../Lib/require.rkt")
(require+ "../Common/board.rkt" board? on-board? token? east-west/c north-south/c stay-on-board? PUT)
(require+ "../Common/actions.rkt" action?)

(provide
 (contract-out
  (check-action
   ;; is the given action legal on this board? 
   (-> board? action? boolean?))
   
  (can-move-and-build?
   ;; can the given token move on this board and then build up a house on this board? 
   (->i ((b board?) (t (b) (and/c token? (on-board? b)))) (r boolean?)))
  
  (check-move
   ;; can the given token move in the specified direction on this board? 
   (->i ((b board?) (t (b) (and/c token? (on-board? b))) (e-w east-west/c) (n-s north-south/c))
        #:pre/name (e-w n-s) "a token can't stay put" (not (and (= e-w PUT) (= n-s PUT)))
        #:pre/name (b t e-w n-s) "stay on board" (stay-on-board? t e-w n-s) 
        (r boolean?)))
  
  (check-build-up
   ;; can the token move to (e-w,n-s) and then build on (b-e-w,b-n-s) on this board?
   ;; ASSUME check-move has been called to make sure the token move is okay 
   (->i ((b board?)
         (t (b) (and/c token? (on-board? b)))
         (e-w east-west/c) (n-s north-south/c)
         (b-e-w east-west/c) (b-n-s north-south/c))
        #:pre/name (e-w n-s) "a token can't stay put"
        (not (and (= e-w PUT) (= n-s PUT)))
        #:pre/name (b t e-w n-s) "stay on board"
        (stay-on-board? t e-w n-s)
        #:pre/name (b-e-w b-n-s) "build not here"
        (not (and (= b-e-w PUT) (= b-n-s PUT)))
        #:pre/name (t e-w n-s b-e-w b-n-s) "build on board"
        (stay-on-board? (move-token t e-w n-s) b-e-w b-n-s)
        (r boolean?)))))

;; ---------------------------------------------------------------------------------------------------
(require- "../Common/board.rkt" board? on-board? token? east-west/c north-south/c stay-on-board? PUT)

(module+ test
  (require (submod "../Common/board.rkt" test))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (can-move-and-build? b t)
  (for/or ((n (all-directions-to-neighbors t)))
    (match-define `(,e-w ,n-s) n)
    (and (check-move b t e-w n-s)
         (for/or ((n (all-directions-to-neighbors (move-token t e-w n-s))))
           (match-define `(,b-e-w ,b-n-s) n)
           (check-build-up b t e-w n-s b-e-w b-n-s)))))

(define (check-move b t e-w n-s)
  (and (location-free-of-token? b t e-w n-s) (check-height-delta? b t e-w n-s)))

(define (check-build-up b t e-w n-s b-e-w b-n-s)
  (define new-token (move-token t e-w n-s))
  (define new-board (move b t e-w n-s))
  (and (location-free-of-token? new-board new-token b-e-w b-n-s)
       (< (height-of new-board new-token b-e-w b-n-s) MAX-HEIGHT)))

;; Board Token E-W-Dir N-S-Dir -> Boolean
;; is the up-delta <= 1 or is it going down?
(define (check-height-delta? b t e-w n-s)
  (define z0 (height-of b t))
  (define z1 (height-of b t e-w n-s))
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
       [4 2o 1o]])
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
  
  (checker #t check-build-up b2 ("b" 0 1) EAST SOUTH WEST PUT)
  (checker #f check-build-up b1-before ("x" 1 0) WEST PUT PUT SOUTH)
  
  (checker #t can-move-and-build? b1-before ("x" 2 0))
  (checker #f can-move-and-build? (board-move (list 0 "x")) ("x" 1 0))
  
  (check-fail check-move (move b2 (token "b" 0 1) PUT SOUTH) ("b" 0 1) EAST PUT))
