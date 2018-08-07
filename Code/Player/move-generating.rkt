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
(require+ "../Common/board.rkt" board? on? token? east-west/c north-south/c)

(provide
 ;; type Tree 
 tree?

 ;; type Action
 action?
 action
 
 (contract-out

  #;
  (action
   (-> token? east-west/c north-south/c east-west/c north-south/c action?))

  (tree-actions
   (-> tree? (listof action?)))

  (generate
   ;; the game tree starting from this board with player making the first move, other responds
   ;; ASSUME player and other are the two tokens on this board  
   (->i ((b board?) (p (b) (and/c string? (on? b))) (o (b) (and/c string? (on? b)))) (r tree?)))
  
  (step
   ;; the game tree for a specific action by token t, yielding the decision node for the other player
   (->i ((gt tree?) (a action?))
        #:pre/name (gt a) "legitimate move and build" (member a (tree-actions gt))
        (result tree?)))))

;; ---------------------------------------------------------------------------------------------------
(require- "../Common/board.rkt" board? on? token? east-west/c north-south/c)
(require "../Admin/rule-checking.rkt")
(require "../Lib/struct-with.rkt")
(module+ test
  (require (submod "../Common/board.rkt" test))
  (require rackunit))

(struct tree (board actions next))
(struct action (actor e-w-move n-s-move e-w-build n-s-build) #:transparent)
;; GameTree = (tree Board [Listof Action] (Board -> GameTree))
;; Action   = [action Token EWDIR NSDIR EWDIR NSDIR]
;;                     t moves e-w & n-s, then builds in the specified directions

(define (generate board player other)
  (define actions
    (for/fold ((actions '())) ((t (named-tokens board player)))
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

(define (step gt a)
  (match-define (action t e-w-move n-s-move e-w-build n-s-build) a)
  (with tree gt
        (define new-t       (move-token t e-w-move n-s-move))
        (define move-board  (move board t e-w-move n-s-move))
        (define build-board (build move-board new-t e-w-build n-s-build))
        (next build-board)))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))

  (define-syntax-rule
    (check-generate r sel b player other)
    (check-equal? (sel (generate (let () (define-board name b) name) player other)) r))
  
  (check-generate '() 
                  tree-actions
                  [[1x 2o]
                   [2x 1o]
                   [4  4]]
                  "x" "o")

  (define ((directions f) t)
    (set-count (for/set ((a (tree-actions t))) (f a))))
    
  (check-generate 3
                  (directions (match-lambda [(action _t x y _dx _dy) (list x y)]))
                  [[2o 1x]
                   [2x 1o]
                   [4  4 ]]
                  "o" "x")

  (check-generate 8 
                  (directions (match-lambda [(action _t _x _y dx dy) (list dx dy)]))
                  [[2o 1x]
                   [2x 1o]
                   [4  4 ]]
                  "o" "x")

  (check-generate 17
                  (directions values)
                  [[2o 1x]
                   [2x 1o]
                   [4  4 ]]
                  "o" "x")

  (check-generate 1
                  (directions (match-lambda [(action _t x y _d _e) (list x y)]))
                  [[1x 2o 4]
                   [2x 1o 4]
                   [4  4  2]]
                  "o" "x")
  
  (check-generate (list (action (token "x" 0 0) 1 1 -1 -1) (action (token "x" 0 1) 1 0 -1 0))
                  (compose tree-actions (lambda (b) (step b (action (token "o" 1 1) 1 1 1 0))))
                  [[1x 2o 4]
                   [2x 1o 4]
                   [4  4  2]]
                  "o" "x")
  )
