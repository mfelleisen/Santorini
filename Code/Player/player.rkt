#lang racket

(require "../Lib/require.rkt")
(require+ "../Common/board.rkt" board? in-range?)
(require+ "../Common/actions.rkt" action?)

(provide
 (contract-out 
  (player%
   (class/c
    [other
     ;; this player finds out the name of the other player 
     (->m string? any)]
    [placement
     ;; this player picks the placement of a worker, given a list of prior placements 
     (->m (listof (list/c in-range? in-range?)) (list/c in-range? in-range?))]
    [take-turn
     ;; this player picks its next action 
     (->m board? action?)]))))
    

;; ---------------------------------------------------------------------------------------------------
(require- "../Common/board.rkt" board? in-range?)
(require- "../Common/actions.rkt" action?)
(require "strategy.rkt")

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define player%
  (class object% (init-field name)
    (super-new)

    (field [other-name ""])
    
    ;; String -> Void
    ;; inform this player of the name of the other player 
    (define/public (other name) (set! other-name name))

    ;; [Listof (list N N)] -> (List N N)
    ;; create a new token with distinct locations from the given ones
    (define/public (placement list-of-places)
      (cond
        [(empty? list-of-places) (list 0 0)]
        [else (define max-x (first (argmax first list-of-places)))
              (define max-y (first (argmax second list-of-places)))
              (list (+ 1 max-x) (+ 1 max-y))]))
      
    ;; Board -> Action 
    (define/public (take-turn board)
      (safe-strategy board name other-name))))