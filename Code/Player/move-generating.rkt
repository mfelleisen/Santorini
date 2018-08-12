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
(require "../Lib/require.rkt")

(provide
 ;; type Tree 
 tree?
 
 (contract-out
  (tree-actions
   ;; retrieve the currently available actions; at least (giving-up)
   (-> tree? (and/c cons? (listof action?))))

  (generate
   ;; the game tree starting from this board with player making the first move, other responds
   ;; ASSUME player and other are the two workers on this board  
   (->i ((b board?) (p (b) (and/c string? (on? b))) (o (b) (and/c string? (on? b)))) (r tree?)))
  
  (step
   ;; the game tree for a specific action by worker t, yielding the decision node for the other player
   (->i ((gt tree?) (a action?))
        #:pre/name (gt a) "legitimate move and build" (member a (tree-actions gt))
        (result tree?)))))

;; ---------------------------------------------------------------------------------------------------
(require "../Common/board.rkt")
(require "../Common/worker.rkt")
(require "../Common/directions.rkt")
(require "../Common/actions.rkt")
(require "../Common/rule-checking.rkt")
(require "../Lib/struct-with.rkt")
(module+ test
  (require (submod "../Common/board.rkt" test))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct tree (board actions next) #:transparent)
;; GameTree = (tree Board [Listof Action] (Board -> GameTree))

(define (generate board player other)
  (define actions
    (for/fold ((actions '())) ((t (named-workers board player)))
      (for/fold ((actions actions)) ((n (all-directions-to-neighbors board t)))
        (match-define `(,e-w-move ,n-s-move) n)
        (cond
          [(not (check-move board t e-w-move n-s-move))
           actions]
          [(is-move-a-winner? board t e-w-move n-s-move)
           (cons (winning-move t e-w-move n-s-move) actions)]
          [else
           [define new-board (move board t e-w-move n-s-move)]
           (for/fold ([actions actions]) ((n (all-directions-to-neighbors new-board t)))
             (match-define `(,e-w-build ,n-s-build) n)
             (if (check-build-up board t e-w-move n-s-move e-w-build n-s-build)
                 (cons (move-build t e-w-move n-s-move e-w-build n-s-build) actions)
                 actions))]))))
  (define next (lambda (board) (generate board other player)))
  (tree board (if (empty? actions) (list (giving-up)) actions) next))

(define (step gt a)
  (with tree gt (next (apply-action board a))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))

  (define-syntax-rule
    (check-generate r sel b player other)
    (check-equal? (sel (generate (let () (define-board name b) name) player other)) r))
  
  (check-generate (list (giving-up))
                  tree-actions
                  [[1x1 2o1]
                   [2x2 1o2]
                   [4   4]]
                  "x" "o")

  (define ((directions f) t)
    (set-count (for/set ((a (tree-actions t))) (f a))))
    
  (check-generate 3
                  (directions (match-lambda [(move-build _t x y _dx _dy) (list x y)]))
                  [[2o1 1x1]
                   [2x2 1o2]
                   [4   4 ]]
                  "o" "x")

  (check-generate 17
                  (directions values)
                  [[2o1 1x1]  ;; (2,0) -> 1.1 1.2 1.3 3.0
                   [2x2 1o2]  ;; (2,1) -> 3.2 2.2 3.2 2.0 3.0
                   [4   4 ]] ;; (2,2) -> 3.2 3.1 3.3 2.1 3.1
                  "o" "x")

  (check-generate 1
                  (directions (match-lambda [(move-build _t x y _d _e) (list x y)]))
                  [[1x1 2o1 4]
                   [2x2 1o2 4]
                   [4  4    2]]
                  "o" "x")
  
  (check-generate (list (move-build (worker "x1") EAST SOUTH WEST NORTH)
                        (move-build (worker "x2") EAST PUT WEST PUT))
                  (compose tree-actions
                           (lambda (b) (step b (move-build (worker "o2") EAST SOUTH EAST SOUTH))))
                  [[1x1 2o1 4]
                   [2x2 1o2 4]
                   [4   4   2]]
                  "o" "x")

  (check-generate (list (winning-move (worker "o2") EAST SOUTH))
                  tree-actions
                  [[1x1 2o1 4]
                   [2x2 2o2 4]
                   [4   4   3]]
                  "o" "x"))
