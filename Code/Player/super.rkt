#lang racket

;; a player super class that implements the protocol fields (ARGH!) and
;; reasonable default methods for the interface

;; The class comes with hooks for dealing with the beginning of a game
;; especially which strategy to play. 

(require "../Common/player-interface.rkt")

(provide
 (contract-out 
  (super% player%/c)))
 
;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod "../Common/board.rkt" test-support))
  (require "../Lib/with-output-to-dev-null.rkt")
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define super%
  (class object% (init-field name (other "aaaaxxxx"))
    (super-new)

    (field
      [playing-as-has-been-called-once #false]
      [other-name-has-been-called      #false]
      [placement-has-been-called-once  #false]
      [placement-has-been-called-twice #false])

    ;; -----------------------------------------------------------------------------
    ;; pre-tournament 
    (define/public (playing-as my-new-name)
      (set! name my-new-name))

    ;; -----------------------------------------------------------------------------
    ;; tournament 
    ;; strategy%/c
    (field [strategy #f])

    (define/pubment (other-name oname)
      (set! other oname)
      (inner (void) other-name oname))

    (define/public (placement list-of-places)
      (send strategy initialization list-of-places))
    
    (define/public (take-turn board)
      (send strategy take-turn board))
    
   (define/public (end-of-game results)
     (pretty-print results))))
