#lang racket

(provide
 ;; a contract that describes the player class's interface to the administrator 
 player%/c

 ;; a contract that describes the player object's interface to the administrator 
 player/c

 ;; a contract that describes the player class's interface to the administrator PLUS its protocol 
 player-protocol%/c

 ;; the rest of the interface
 (all-from-out "results.rkt")
 (all-from-out "actions.rkt")
 (all-from-out "placements.rkt"))

;; ---------------------------------------------------------------------------------------------------
(require "actions.rkt")
(require "placements.rkt")
(require "results.rkt")

(module+ test
  (require (submod "../Common/board.rkt" test-support))
  (require "../Lib/with-output-to-dev-null.rkt")
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------

(define player%/c
  (class/c
    ; #:opaque
    (init-field (name good-player-name?) (other good-player-name?))

    ;; protocol: {playing-as | other-name}^1 -> placement-once -> placement-twice -> take-turn* -> end
    (field
     [playing-as-has-been-called-once boolean?]
     [other-name-has-been-called      boolean?]
     [placement-has-been-called-once  boolean?]
     [placement-has-been-called-twice boolean?])

    ;; IF name is already taken by some other player, this method is called with a replacement name
    (playing-as (->m good-player-name? any/c))
    ;; name of the opponent of this player
    (other-name (->m good-player-name? any/c))
    ;; compute the placement of this player's next worker, given the placement of other workers
    (placement (->m placements/c place/c))
    ;; compute the next action that this player can take for the given board
    (take-turn (->m board? action?))
    ;; inform this player of the outcome of all games in a tournament
    (end-of-game (->m result*/c any/c))))

(define player/c (instanceof/c player%/c))

(define player-protocol%/c
   (class/c
    (playing-as (->i ([this any/c][nu (this) (protocol-as this)]) (r any/c)))
    (other-name (->i ([this any/c][n  (this) (protocol-other this)]) (r any/c)))
    (placement  (->i ([this any/c][pl (this) (protocol-placement this)]) (r any/c)))
    (take-turn  (->i ([this any/c][b  (this) (protocol-placements-set? this)]) (r any/c)))))

(define player+protocl%/c (and/c player%/c player-protocol%/c))

(define player+protocl/c  (instanceof/c player+protocl%/c))

;; ---------------------------------------------------------------------------------------------------
;; protocol functions 

(define ((protocol-as this) _)
  ;; no checking yet 
  (set-field! playing-as-has-been-called-once this #true))

(define ((protocol-other this) _)
  ;; no checking yet
  (set-field! placement-has-been-called-once  this #false)
  (set-field! other-name-has-been-called this #true))

(define ((protocol-placements-set? this) _)
  (set-field! placement-has-been-called-once this #false)
  #true)

(define ((protocol-placement this) placements)
  (define me    (get-field name this))
  (define once  (get-field placement-has-been-called-once this))
  (cond
    [once 
     (unless (placed-at-least-one placements me)
       (log-error "during the second call ~a must have placed one worker" me))
     (define you (get-field other this))
     (unless (placed-at-least-one placements you)
       (log-error "during the second call ~a must have placed one worker" you))
     (and (placed-at-least-one placements me) (placed-at-least-one placements you))]
    [else ;; first call:
     (set-field! placement-has-been-called-once this #true)
     (when (placed-at-least-one placements me)
       (log-error "during the first call ~a must not have placed a worker" me))
     (not (placed-at-least-one placements me))]))

(define (placed-at-least-one placements you)
  (for/first ((p placements) #:when (string=? (first p) you)) p))


;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))

  (check-false  (placed-at-least-one '() "cd"))
  (check-equal? (placed-at-least-one `(("cd" 0 0)) "cd") '("cd" 0 0))

  ;; -------------------------------------------------------------------------------------------------
  (define/contract mock-player% player-protocol%/c
    (class object% (init-field name other)
      (super-new)
      (field
       [playing-as-has-been-called-once #false]
       [other-name-has-been-called      #false]
       [placement-has-been-called-once  #false]
       [placement-has-been-called-twice #false])
      
      (define/public (playing-as n) (void))
      (define/public (other-name n) (void))
      (define/public (end-of-game n) (void))
      
      (define/public (placement lop) (list 0 0))
      (define/public (take-turn b) (giving-up name))))

  (check-pred cons? (send (new mock-player% [name "x"][other "o"]) placement '()) "test in")

  (define b (cboard [[0x1 0x2][0o1 0o2]]))
  (check-pred giving-up? (send (new mock-player% [name "x"][other "o"]) take-turn b) "test tt")

  (define (protocoled-mocked-player)
    (protocol-placement (new mock-player% [name "x"][other "o"])))
  
  (check-true   ((protocoled-mocked-player) '()))
  (check-equal? (let ([s (protocoled-mocked-player)])
                  (s '()) ;; first, irrelevant call 
                  (s '(("o" 1 1) ("x" 1 2))))
                '("o" 1 1))

  (check-false (let ([s (protocoled-mocked-player)])
                 (s '()) ;; first, irrelevant call 
                 (s '(("o" 1 1) )))
               '("o" 1 1)))

  

