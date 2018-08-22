#lang racket

(require "../Common/player-interface.rkt")

(provide
 (contract-out 
  (player% player%/c)))
 
;; ---------------------------------------------------------------------------------------------------
(require "strategy.rkt")

;; ---------------------------------------------------------------------------------------------------
(define player%
  (class object% (init-field name)
    (super-new)

    (define other-name "")
    (define strategy #f)
    
    (define/public (other other-name)
      (set! other-name other-name)
      (set! strategy (new strategy% [player name][other other-name])))

    (define/public (placement list-of-places)
      (send strategy initialization list-of-places))
    
    (define/public (take-turn board)
      (send strategy take-turn board))))
