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
    (init-field
     (one player/c)
     (two player/c))
    
    (best-of (->m (and/c natural-number/c odd?) (or/c string? terminated?)))
    (play    (->m (or/c string? terminated?)))))))

;; ---------------------------------------------------------------------------------------------------
(require "../Common/player-interface.rkt")
(require "../Lib/xsend.rkt")
(module+ test
  (require "../Player/player.rkt") ;; ??? 
  (require (submod "../Common/board.rkt" test))
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
(define XPLAY:fmt         "the 'play' method failed for ~a\n[~a]")

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
        (xsend one other #:thrown ex-one #:timed-out ex-one two-name)
        (xsend two other #:thrown ex-two #:timed-out ex-two one-name)
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
      (done (terminated winner-name (format fmt bad-guy-name extra))))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))

  (define (make-mock-player%
           lot
           (tt void)
           #:other (oo void)
           #:setup (ss (lambda (_) (begin0 (first lot) (set! lot (rest lot))))))
    (class object% (init-field name) 
      (super-new)
      (define/public (other s) (oo s))
      (define/public (placement _lot) (ss _lot))
      (define/public (take-turn board) (tt board))))

  (define-syntax checker
    (syntax-rules ()
      [(checker r action (args ...) lot tt ...)
       (check-equal?
        (let ()
          [define player% (make-mock-player% lot tt ...)]
          [define player1 (new player% [name "one"])]
          [define player2 (new player% [name "two"])]
          [define referee (new referee% [one player1] [two player2])]
          (with-output-to-dev-null (lambda () (action referee args ...))))
        r)]
      [(checker r action (args ...) lot tt ...)
       (checker r (action) (args ...) lot tt ...)]))
  (define diagonal (build-list 4 (lambda (i) (list i i))))

  (checker #t (lambda (r) (board? (get-field board r))) () diagonal)

  (define board-2-rounds-play
    (cboard
     [[2one1 2two1 3]
      [2one2 2two2 4]
      [2     4    ]
      [3     4    ]]))
  (define actions
    (list (move-build (worker "one2") PUT SOUTH PUT SOUTH) (winning-move (worker "two1") EAST PUT)))
  (define ((stepper actions) b) (begin0 (tee (first actions)) (set! actions (rest actions))))
  (define (tee x) (displayln x) x)

  (define bad-placement (make-list 4 (list 1 1)))
  (define ((givesup n) b) (giving-up n))
  (define (bad-action b) (move-build (worker "one1") EAST SOUTH PUT NORTH))
  (define (div-by-zero b) (/ 1 0))

  (define div0  "/: division by zero")
  (define timed "timed out")

  (define-syntax-rule
    (check-run (m a ...) (stuff ... msg) ...)
    (begin (checker msg send (m a ...) stuff ...) ... ))

  (check-run
   (play-rounds raise board-2-rounds-play)
   (diagonal (stepper actions)         (format WINNING:fmt "two")))
   
  (check-run
   (play)
   (bad-placement                      (terminated "one" (format BAD-PLACEMENT:fmt "two" "")))
   (diagonal (givesup "one")           (format GIVING-UP:fmt "two" "one"))
   (diagonal bad-action                (terminated "one" (format BAD-MOVE:fmt "two" (bad-action '_))))
   (diagonal #:other div-by-zero       (terminated "two" (format XOTHER:fmt "one" "")))
   (diagonal #:setup div-by-zero       (terminated "two" (format XSETUP:fmt "one" "")))
   (diagonal (lambda _ (let L () (L))) (terminated "two" (format XPLAY:fmt "one" timed)))
   (diagonal div-by-zero               (terminated "two" (format XPLAY:fmt "one" div0))))

  (check-run
   (best-of 1)
   (bad-placement                      (terminated "one" (format BAD-PLACEMENT:fmt "two" "")))
   (diagonal (givesup "one")           "two")
   (diagonal bad-action                (terminated "one" (format BAD-MOVE:fmt "two" (bad-action '_))))
   (diagonal #:other div-by-zero       (terminated "two" (format XOTHER:fmt "one" "")))
   (diagonal #:setup div-by-zero       (terminated "two" (format XSETUP:fmt "one" "")))
   (diagonal (lambda _ (let L () (L))) (terminated "two" (format XPLAY:fmt "one" timed)))
   (diagonal div-by-zero               (terminated "two" (format XPLAY:fmt "one" div0))))

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
                  [define p1 (new player-1% [name "one"])]
                  [define p2 (new player-2% [name "two"])]
                  [define re (new referee% [one p1][two p2])]
                  (with-output-to-dev-null (lambda () (send re best-of 3))))
                "one"
                "complete test coverage for referee"))
