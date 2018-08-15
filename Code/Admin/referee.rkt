#lang racket

;; The Referee plays a single game for two players.
;; The class is instanitated with two players. 

(require "referee-interface.rkt")

(provide
 (contract-out
  (referee% referee%/c)))

;; ---------------------------------------------------------------------------------------------------
(require "../Player/player.rkt")
(require "../Common/worker.rkt")
(require "../Common/board.rkt")
(require "../Common/actions.rkt")
(require "../Lib/xsend.rkt")
(module+ test
  (require "../Common/directions.rkt")
  (require (submod "../Common/board.rkt" test))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define-local-member-name play-rounds) ;; make private methods visible within this module 

(define GIVING-UP:fmt "~a, because ~a gave up")
(define WINNING:fmt "~a made a winning move")
(define BAD-PLACEMENT:fmt "~a broke the rules of placing workers")
(define BAD-MOVE:fmt "~a\n ~a broke the rules\n [~e]")

(define XOTHER "the 'other' method failed for ~a")
(define XSETUP "the 'placement' method failed for ~a")
(define XPLAY  "~a won, because the 'play' method failed for ~a\n[~a]")

(define referee%
  (class object% (init-field one two)
    (super-new)

    ;; -----------------------------------------------------------------------------------------------
    ;; SET UP

    ;; [String -> Empty] FormatString(of 1) String -> Empty 
    (define/private ((report done fmt name) . x)
      (done (format fmt name)))

    ;; [String -> Empty] -> (U Board String)
    ;; EFFECT escape with done if target produces a location that is already occupied or xsend fails
    (define/private (setup done)
      ;; (Player String [String -> Empty] Placements -> (values (List Worker [List N N]) Placements))
      ;; get a placement for a specific worker from target
      (define (placement target target-name worker# lot)
        (define ex-t (report done XSETUP target-name))
        (define new-place (xsend target placement #:thrown ex-t #:timed-out ex-t lot))
        (when (memf (lambda (t) (equal? (rest t) new-place)) lot)
          (done (format BAD-PLACEMENT:fmt target-name)))
        (define full-name (string-append target-name (number->string worker#)))
        (values (cons (worker full-name) new-place) (cons (cons target-name new-place) lot)))
      ;; -- IN -- 
      (let*-values ([(                lot) '()]
                    [(player1-worker1 lot) (placement one one-name 1 lot)]
                    [(player2-worker1 lot) (placement two two-name 1 lot)]
                    [(player1-worker2 lot) (placement one one-name 2 lot)]
                    [(player2-worker2 lot) (placement two two-name 2 lot)])
        (init player1-worker1 player1-worker2 player2-worker1 player2-worker2)))
    
    (field
     [one-name (get-field name one)]
     [two-name (get-field name two)]
     [board ;; Board ~~ the initial board, with four workers, two per player 
      (let/ec done
        (define ex-one (report done XOTHER one-name))
        (define ex-two (report done XOTHER two-name))
        (xsend one other #:thrown ex-one #:timed-out ex-one two-name)
        (xsend two other #:thrown ex-two #:timed-out ex-two one-name)
        (setup done))])

    ;; -----------------------------------------------------------------------------------------------
    ;; RUN A GAME 

    ;; -> String
    ;; determine the winner (and the loser)
    (define/public (play)
      (let/ec done
        (if (string? board) board (play-rounds done board))))

    ;; [String -> Empty] Board -> String
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
          [(report done (format XPLAY "~a" one-name (if (exn? bv) (exn-message bv) bv)) two-name)])
        (displayln a)
        (displayln (check-action board a))
        (unless (check-action board a)
          (displayln `(bad action ,a))
          [(report done (format BAD-MOVE:fmt "~a" one-name a) two-name)])
        (displayln (apply-action board a))
        (cond
          [(giving-up? a) (format GIVING-UP:fmt two-name one-name)]
          [(winning-move? a) (format WINNING:fmt one-name)]
          [(move-build? a) (play-rounds (apply-action board a) two two-name one one-name)])))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))

  (define (make-mock-player% lot
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
        (parameterize ([current-output-port #;(current-output-port) (open-output-string)])
          [define player% (make-mock-player% lot tt ...)]
          [define player1 (new player% [name "one"])]
          [define player2 (new player% [name "two"])]
          [define referee (new referee% [one player1] [two player2])]
          (action referee args ...))
        r)]
      [(checker r action (args ...) lot tt ...)
       (checker r (action) (args ...) lot tt ...)]))
  (define diagonal (build-list 4 (lambda (i) (list i i))))

  (checker (format BAD-PLACEMENT:fmt "two") send (play) (make-list 4 (list 1 1)))
  (checker #t (lambda (r) (board? (get-field board r))) () diagonal)
  (checker (format GIVING-UP:fmt "two" "one") send (play) diagonal (lambda (b) (giving-up "one")))

  (define bad-action (move-build (worker "one1") EAST SOUTH PUT NORTH))
  (checker (format BAD-MOVE:fmt "one" "two" bad-action) send (play) diagonal (lambda (b) bad-action))

  (define-board board-2-rounds-play
    [[2one1 2two1 3]
     [2one2 2two2 4]
     [2     4    ]
     [3     4    ]])
  (define actions
    (list (move-build (worker "one2") PUT SOUTH PUT SOUTH) (winning-move (worker "two1") EAST PUT)))
  (define (stepper b) (begin0 (first actions) (set! actions (rest actions))))
  (checker (format WINNING:fmt "two") send (play-rounds raise board-2-rounds-play) diagonal stepper)

  ;; --- failed method calls
  (checker (format XOTHER "one") send (play) diagonal #:other (lambda _ (/ 1 0)))
  (checker (format XSETUP "one") send (play) diagonal #:setup (lambda (b) (/ 1 0)))
  (checker (format XPLAY "two" "one" "/: division by zero") send (play) diagonal (lambda _ (/ 1 0)))
  (checker (format XPLAY "two" "one" "timed out") send (play) diagonal (lambda _ (let L () (L)))))


;; ---------------------------------------------------------------------------------------------------
#;
(module+ main
  
  (define admin
    (new referee%
         [one (new player% [name "mf"])]
         [two (new player% [name "cd"])]))

  (time-out-limit 1.0)
  (send admin play))
