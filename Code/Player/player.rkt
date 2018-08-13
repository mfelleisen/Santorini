#lang racket

(require "../Lib/require.rkt")
(require+ "../Common/player-interface.rkt" player%/c)
(require+ "../Common/board.rkt" board? in-range?)
(require+ "../Common/actions.rkt" action?)

(provide
 (contract-out 
  (player% player%/c)))
 
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

    (define other-name "")
    
    (define/public (other name) (set! other-name name))

    (define/public (placement list-of-places)
      ;; this should probably use a strategy
      (cond
        [(empty? list-of-places) (list 0 0)]
        [else (define max-x (apply max (map second list-of-places)))
              (define max-y (apply max (map third  list-of-places)))
              (list (+ 1 max-x) (+ 1 max-y))]))
    
    (define/public (take-turn board)
      (safe-strategy board name other-name))))
