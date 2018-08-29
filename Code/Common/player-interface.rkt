#lang racket

(provide
 ;; a contract that describes the player class's interface to the administrator 
 player%/c

 ;; a contract that describes the player object's interface to the administrator 
 player/c

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

(define PLAYER-NAME #px"[a-z]+")
(define (good-player-name? name)
  (regexp-match PLAYER-NAME name))

(define player%/c
  (class/c
   #:opaque
   (init-field (name (and/c string? good-player-name?)) (other (and/c string? good-player-name?)))

   ;; protocol: {playing-as | other-name}^1 -> placement-once -> placement-twice -> take-turn* -> end
   (field
    [playing-as-has-been-called-once boolean?]
    [other-name-has-been-called      boolean?]
    [placement-has-been-called-once  boolean?]
    [placement-has-been-called-twice boolean?])

   (playing-as
    ;; IF name is already taken by some other player, this method is called with a replacement name 
    (->i ([this any/c][nu (this) (and/c string? (protocol-as this))]) (r any/c)))
   (other-name
    ;; name of opponent of this player 
    (->i ([this any/c] [n (this) (and/c string? (protocol-other this))]) (r any/c)))
   (placement
    ;; compute the placement of this player's next worker, given the placement of other workers
    ;; ASSUME this player knows where it places its players 
    (->i ([this any/c][pl (this) (and/c placements/c (placement-protocol/c this))]) (r place/c)))
   (take-turn
    ;; compute the next action that this player can take for the given board 
    (->i ([this any/c][b (this) (and/c board? (protocol-placements-set? this))]) (r action?)))
   (end-of-game
    ;; inform this player of the outcome of all games in a tournament 
    (->i ([this any/c] [r (this) (and/c result*/c #;anything?)]) (x any/c)))))

(define player/c (instanceof/c player%/c))

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
  #true
  #;
  (and (get-field placement-has-been-called-once this)
       (get-field placement-has-been-called-twice this)))

(define ((placement-protocol/c this) placements)
  (define me    (get-field name this))
  (define once  (get-field placement-has-been-called-once this))
  (cond
    [once 
     (unless (placed-at-least-one placements me)
       (displayln `(during the second call ,me must have placed one worker) (current-error-port)))
     (define you (get-field other this))
     (unless (placed-at-least-one placements you)
       (displayln `(during the second call ,you must have placed one worker) (current-error-port)))
     (and (placed-at-least-one placements me) (placed-at-least-one placements you))]
    [else ;; first call:
     (set-field! placement-has-been-called-once this #true)
     (when (placed-at-least-one placements me)
       (displayln `(during the first call ,me must not have placed a worker) (current-error-port)))
     (not (placed-at-least-one placements me))]))

(define (placed-at-least-one placements you)
  (for/first ((p placements) #:when (string=? (first p) you)) p))


;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))

  (check-false  (placed-at-least-one '() "cd"))
  (check-equal? (placed-at-least-one `(("cd" 0 0)) "cd") '("cd" 0 0))

  ;; -------------------------------------------------------------------------------------------------
  (define/contract mock-player% player%/c
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
    (placement-protocol/c (new mock-player% [name "x"][other "o"])))
  
  (check-true   ((protocoled-mocked-player) '()))
  (check-equal? (let ([s (protocoled-mocked-player)])
                  (s '()) ;; first, irrelevant call 
                  (s '(("o" 1 1) ("x" 1 2))))
                '("o" 1 1))

  (check-false (let ([s (protocoled-mocked-player)])
                 (s '()) ;; first, irrelevant call 
                 (s '(("o" 1 1) )))
               '("o" 1 1)))

  

