#lang racket

;; The Referee plays a single game for two players.
;; The class is instanitated with two players.

(provide
 ;; type Terminated = (terminated String String)
 terminated
 terminated?

 (contract-out
  (referee%
   (class/c
    (init-field (one any/c #; player/c) (two any/c #; player/c))
    
    (best-of (->i ((this any/c) (n (and/c natural-number/c odd?)))
                  #:pre/name (this) "distinct names" (distinct? this)
                  (r (or/c string? terminated?))))
    (play    (->i ((this any/c))
                  #:pre/name (this) "distinct names" (distinct? this)
                  (r (or/c string? terminated?))))))))

(define (distinct? this)
  (define player1 (get-field one this))
  (define player2 (get-field two this))
  (not (string=? (get-field name player1) (get-field name player2))))

;; ---------------------------------------------------------------------------------------------------
(require "../Common/player-interface.rkt")
(require "../Lib/xsend.rkt")
(module+ test
  (require "../Player/player.rkt") ;; ??? 
  (require (submod "../Common/board.rkt" test-support))
  (require "../Lib/with-output-to-dev-null.rkt")
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct terminated [winner message] #:transparent)

(define-local-member-name play-rounds) ;; make private methods visible within this module 

(define GIVING-UP:fmt     "~a, because ~a gave up")
(define WINNING:fmt       "~a, it made a winning move")

(define BAD-PLACEMENT:fmt "~a broke the rules of placing workers~a")
(define BAD-MOVE:fmt      "~a broke the rules\n [~e]")
(define XOTHER:fmt        "~a's 'other' method failed~a")
(define XSETUP:fmt        "~a failed with the 'placement' method ~a")
(define XPLAY:fmt         "the 'take-turn' method failed for ~a\n[~a]")

(define referee%
  (class object% (init-field one two)
    (super-new)

    ;; -----------------------------------------------------------------------------------------------
    ;; SET UP
    
    ;; [String -> Empty] -> (U Board String)
    ;; EFFECT escape with done if target produces a location that is already occupied or xsend fails
    (define/private (setup done)
      ;; (Player String [String -> Empty] Placements -> (values (List Worker [List N N]) Placements))
      ;; get a placement for a specific worker from target
      (define (placement target target-name worker# winner-name lot)
        (define ex-t (report done XSETUP:fmt target-name "" winner-name))
        (define new-place (xsend target placement #:thrown ex-t #:timed-out ex-t lot))
        (when (memf (lambda (t) (equal? (rest t) new-place)) lot)
          [(report done BAD-PLACEMENT:fmt target-name "" winner-name)])
        (define full-name (string-append target-name (number->string worker#)))
        (values (cons (worker full-name) new-place) (cons (cons target-name new-place) lot)))
      ;; -- IN --
      (let*-values ([(                lot) '()]
                    [(player1-worker1 lot) (placement one one-name 1 two-name lot)]
                    [(player2-worker1 lot) (placement two two-name 1 one-name lot)]
                    [(player1-worker2 lot) (placement one one-name 2 two-name lot)]
                    [(player2-worker2 lot) (placement two two-name 2 one-name lot)])
        (init player1-worker1 player1-worker2 player2-worker1 player2-worker2)))
    
    (field
     [one-name (get-field name one)]
     [two-name (get-field name two)]
     [board ;; (U Terminated Board) ~~ the initial board, with four workers, two per player 
      (let/ec done
        (define ex-one (report done XOTHER:fmt one-name "" two-name))
        (define ex-two (report done XOTHER:fmt two-name "" one-name))
        (xsend one other-name #:thrown ex-one #:timed-out ex-one two-name)
        (xsend two other-name #:thrown ex-two #:timed-out ex-two one-name)
        (setup done))])

    ;; -----------------------------------------------------------------------------------------------
    ;; RUN A GAME 

    ;; N -> (U String Terminated)
    ;; who is going to win most games 
    ;; a rule-violating, failing or timed-out player loses regardless of prior rounds
    (define/public (best-of n)
      (define n-winners (+ (quotient n 2) 1))
      (define one-won (regexp (string-append "^" one-name)))
      
      (let/ec done
        (let loop ([one# 0][two# 0])
          (cond
            [(>= one# n-winners) one-name]
            [(>= two# n-winners) two-name]
            [else (define outcome (play done))
                  (if (regexp-match one-won outcome)
                      (loop (+ one# 1) two#)
                      (loop one#       (+ two# 1)))]))))
    
    ;; {[Terminate -> Empty]} -> String
    ;; determine the winner (and the loser)
    (define/public (play (done #false))
      (cond
        [(terminated? board) (if done (done board) board)]
        [done (play-rounds done board)]
        [else (let/ec done (play-rounds done board))]))

    ;; [Terminated -> Empty] Board -> String
    ;; EFFECT escape with done if
    ;; -- a player's external method does not return properly,
    ;; -- times out, or
    ;; -- produces an action invalid for the current board 
    (define/public (play-rounds done board0)
      (struct bad (value))
      (let play-rounds ([board board0][one one][one-name one-name][two two][two-name two-name])
        (define a (xsend one take-turn #:thrown bad #:timed-out (lambda () (bad "timed out")) board))
        (when (bad? a)
          (define bv (bad-value a))
          [(report done XPLAY:fmt one-name (if (exn? bv) (exn-message bv) bv) two-name)])
        (displayln a)
        (unless (check-action board a)
          (displayln `(bad action ,a))
          [(report done BAD-MOVE:fmt one-name a two-name)])
        (displayln (apply-action board a))
        (cond
          [(giving-up? a) (format GIVING-UP:fmt two-name one-name)]
          [(winning-move? a) (format WINNING:fmt one-name)]
          [(move-build? a) (play-rounds (apply-action board a) two two-name one one-name)])))
    
    ;; [String -> Empty] FormatString(of 1) String -> Empty 
    (define/private ((report done fmt bad-guy-name extra winner-name) . _)
      (define msg (format fmt bad-guy-name extra))
      (done (terminated winner-name msg)))))

;; -------------------------------------------------------------------------------------------------
;; testing support

(module* test-support #f

  ;; player 'one' plays against player 'two'

  (provide
   ;; SYNTAX (checker* (method method-args ...) (mock-args ... expected) ...)
   checker*

   ;; SYNTAX
   #;(checker expected (pre-action ...) (args ...) lo-placements
              ;; optional args: 
              [take-turn-f] [#:other other-f] [#:setup placement-f])
   checker

   set-up-ref-and-play
   make-mock-player%)

  ;; -------------------------------------------------------------------------------------------------
  (require (submod ".."))
  (require "../Lib/with-output-to-dev-null.rkt")
  (require rackunit)
  ;; -------------------------------------------------------------------------------------------------
  
  (define-syntax-rule
    (checker* (method method-args ...) (mock-args ... expected) ...)
    (begin (checker expected (send) (method method-args ...) mock-args ...) ... ))
  
  (define-syntax checker
    (syntax-rules ()
      [(checker expected (pre-action ...) (args ...) lot tt ...)
       (check-equal?
        (let ([player% (make-mock-player% lot tt ...)])
          (set-up-ref-and-play player% player% (lambda (ref) (pre-action ... ref args ...))))
        expected)]))

  (define (set-up-ref-and-play pl-1-% pl-2-% action)
    [define player1 (new pl-1-% [name "one"][other "two"])]
    [define player2 (new pl-2-% [name "one"][other "one"])]
    (send player2 playing-as "two")
    (with-output-to-dev-null #:error-port (open-output-string) 
      (lambda () (action (new referee% [one player1] [two player2])))))

  (define (make-mock-player%
           lot
           (tt void)
           #:other (oo void)
           #:setup (ss (lambda (_) (begin0 (first lot) (set! lot (rest lot))))))
    (class object% (init-field name (other "aaaxxx")) 
      (super-new)
      (field
       [playing-as-has-been-called-once #false]
       [other-name-has-been-called      #false]
       [placement-has-been-called-once  #false]
       [placement-has-been-called-twice #false])
      (define/public (playing-as new-name) (set! name new-name))
      (define/public (other-name s) (oo s))
      (define/public (placement _lot) (ss _lot))
      (define/public (take-turn board) (tt board))
      (define/public (end-of-game results) "the referee does not call this method"))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  (require (submod ".." test-support))

  (check-exn exn:fail:contract?
             (lambda ()
               (with-output-to-dev-null #:error-port (open-output-string)
                 (lambda ()
                   (define one (new player% [name "christos-1"]))
                   (define two (new player% [name "christos"]))
                   (new referee% [one one] [two two]))))
             "this test belongs into player-interface, but good enough")

  (check-exn #px"distinct names"
             (lambda ()
               (with-output-to-dev-null #:error-port (open-output-string)
                 (lambda ()
                   (define one (new player% [name "christos"]))
                   (define two (new player% [name "christos"]))
                   (define ref (new referee% [one one] [two two]))
                   (send ref play)))))

  ;; -------------------------------------------------------------------------------------------------
  (define diagonal (build-list 4 (lambda (i) (list i i))))
  
  (define board-diagonal
    (cboard
     [[0one1]
      [0       0two1]
      [0       0       0one2]
      [0       0       0       0two2]]))

  (checker board-diagonal (get-field board) () diagonal)

  ;; -------------------------------------------------------------------------------------------------
  (define board-2-rounds-play
    (cboard
     [[2one1 2two1 3]
      [2one2 2two2 4]
      [2     4    ]
      [3     4    ]]))
  (define actions
    (list (move-build (worker "one2") PUT SOUTH PUT SOUTH) (winning-move (worker "two1") EAST PUT)))
  (define ((stepper actions) b) (begin0 (first actions) (set! actions (rest actions))))
  
  (define bad-placement (make-list 4 (list 1 1)))
  (define ((givesup n) b) (giving-up n))
  (define (bad-action b) (move-build (worker "one1") EAST SOUTH PUT NORTH))
  (define (div-by-zero b) (/ 1 0))

  (define div0  "/: division by zero")
  (define timed "timed out")
  
  (checker*
   (play-rounds raise board-2-rounds-play)
   (diagonal (stepper actions)         (format WINNING:fmt "two")))
   
  (checker*
   (play)
   (bad-placement                      (terminated "one" (format BAD-PLACEMENT:fmt "two" "")))
   (diagonal (givesup "one")           (format GIVING-UP:fmt "two" "one"))
   (diagonal bad-action                (terminated "one" (format BAD-MOVE:fmt "two" (bad-action '_))))
   (diagonal #:other div-by-zero       (terminated "two" (format XOTHER:fmt "one" "")))
   (diagonal #:setup div-by-zero       (terminated "two" (format XSETUP:fmt "one" "")))
   (diagonal (lambda _ (let L () (L))) (terminated "two" (format XPLAY:fmt "one" timed)))
   (diagonal div-by-zero               (terminated "two" (format XPLAY:fmt "one" div0))))

  (checker*
   (best-of 1)
   (bad-placement                      (terminated "one" (format BAD-PLACEMENT:fmt "two" "")))
   (diagonal (givesup "one")           "two")
   (diagonal bad-action                (terminated "one" (format BAD-MOVE:fmt "two" (bad-action '_))))
   (diagonal #:other div-by-zero       (terminated "two" (format XOTHER:fmt "one" "")))
   (diagonal #:setup div-by-zero       (terminated "two" (format XSETUP:fmt "one" "")))
   (diagonal (lambda _ (let L () (L))) (terminated "two" (format XPLAY:fmt "one" timed)))
   (diagonal div-by-zero               (terminated "two" (format XPLAY:fmt "one" div0))))

  ;; -------------------------------------------------------------------------------------------------
  (define (actions1 one)
    `(,(move-build (worker (string-append one "1")) PUT SOUTH PUT SOUTH) ,(giving-up one)))
  (define (actions2 two)
    `(,(move-build (worker (string-append two "2")) PUT SOUTH PUT SOUTH)))
  (define stepper1
    (let ((one "one"))
      (stepper (append (actions2 one) (actions1 one) (actions2 one) (actions2 one) (actions1 one)))))
  (define stepper2
    (let ((two "two"))
      (stepper (append (actions1 two) (actions2 two) (actions1 two) (actions2 two) (actions2 two)))))
  
  (check-equal? (let ()
                  [define player-1% (make-mock-player% '((0 0) (1 1)) stepper1)]
                  [define player-2% (make-mock-player% '((2 2) (3 3)) stepper2)]
                  (set-up-ref-and-play player-1% player-2% (lambda (ref) (send ref best-of 3))))
                "one"
                "complete test coverage for referee"))
