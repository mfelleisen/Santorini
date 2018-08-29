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
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------

(define PLAYER-NAME #px"[a-z]+")
(define (good-player-name? name)
  (regexp-match PLAYER-NAME name))

(define player%/c
  (class/c
   #:opaque
   (init-field (name (and/c string? good-player-name?)) (other (and/c string? good-player-name?)))
   (playing-as
    ;; IF name is already taken by some other player, this method is called with a replacement name 
    (->m string? any))
   (other-name
    ;; name of opponent of this player 
    (->m string? any))
   (placement
    ;; compute the placement of this player's next worker, given the placement of other workers
    ;; ASSUME this player knows where it places its players 
    (->i ([this any/c][pl (this) (and/c placements/c (placement-protocol/c this))])
         (r place/c)))
   (take-turn
    ;; compute the next action that this player can take for the given board 
    (->m board? action?))
   (end-of-game
    ;; inform this player of the outcome of all games in a tournament 
    (->m result*/c any/c))))

(define player/c (instanceof/c player%/c))


(define ((placement-protocol/c this) placements)
  (define me (get-field name this))
  (cond
    ;; second call: 
    [(and (>= (string-length me) 2) (string=? (substring me 0 2) "1-"))
     (define old-me (substring me 2))
     (set-field! name this old-me)
     (define you (get-field other this))
     (unless (placed-at-least-one placements old-me)
       (displayln `(during the second call ,old-me must have placed one worker) (current-error-port)))
     (unless (placed-at-least-one placements you)
       (displayln `(during the second call ,you must have placed one worker) (current-error-port)))
     (and (placed-at-least-one placements old-me)
          (placed-at-least-one placements you))]
    ;; first call: 
    [else
     (set-field! name this (string-append "1-" me))
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

  

