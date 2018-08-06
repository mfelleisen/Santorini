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

(provide
 ;; type Tree 
 tree?

 (contract-out
  (generate
   ;; the game tree starting from this board with player making the first move, other responds
   ;; ASSUME player and other are the two tokens on this board  
   (-> board? string? string? tree?))
  
  (step
   ;; the game tree for a specific action by token t, yielding the decision node for the other player
   (->i ((gt tree?)
         (t token?)
         (e-w-move direction/c)  (n-s-move direction/c)
         (e-w-build direction/c) (n-s-build direction/c))
        #:pre/name (gt t e-w-move n-s-move e-w-build n-s-build) "legitimate move and build"
        (let ((the-action (action t e-w-move n-s-move e-w-build n-s-build)))
          (ormap (lambda (a) (equal? a action)) (tree-actions gt)))
        (result tree?)))))

;; ---------------------------------------------------------------------------------------------------
(require (except-in "board.rkt" board? on-board?))
(require (except-in "token.rkt" token? direction/c stay-on-board?))
(require "rule-checking.rkt")
(require "../Lib/struct-with.rkt")
(module+ test
  (require (submod "board.rkt" test))
  (require rackunit))

(struct tree (board actions next))
(struct action (actor e-w-move n-s-move e-w-build n-s-build) #:transparent)
;; GameTree = (tree Board [Listof Action] (Board -> GameTree))
;; Action   = [action Token EWDIR NSDIR EWDIR NSDIR]
;;                     t moves e-w & n-s, then builds in the specified directions

(define (generate board player other)
  (define actions
    (for/fold ((actions '())) ((t (board->tokens board player)))
      (for/fold ((actions actions)) ((n (all-directions-to-neighbors t)))
        (match-define `(,e-w-move ,n-s-move) n)
        (cond
          [(not (check-move board t e-w-move n-s-move)) actions]
          [else
           [define new-t   (move-token t e-w-move n-s-move)]
           [define b-moved (move board t e-w-move n-s-move)]
           (for/fold ([actions actions]) ((n (all-directions-to-neighbors new-t)))
             (match-define `(,e-w-build ,n-s-build) n)
             (if (check-build-up b-moved new-t e-w-build n-s-build)
                 (cons (action t e-w-move n-s-move e-w-build n-s-build) actions)
                 actions))]))))
  (tree board actions (lambda (board) (generate board other player))))

(define (step gt t e-w-move n-s-move e-w-build n-s-build)
  (with tree gt
        (define new-t       (move-token t e-w-move n-s-move))
        (define move-board  (move board t e-w-move n-s-move))
        (define build-board (build move-board new-t e-w-build n-s-build))
        (next build-board)))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  
  (define-syntax-rule
    (checker r f b (c tx ty) arg ...)
    (check-equal? (f b (token c tx ty) arg ...) r))
  )

