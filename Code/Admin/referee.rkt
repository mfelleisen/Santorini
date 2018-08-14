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

(define referee%
  (class object% (init-field one two)
    (super-new)

    ;; -- set up --- 

    (field
     [one-name (get-field name one)]
     [two-name (get-field name two)])
    
    (send one other two-name)
    (send two other one-name)
    
    ;; -> (U Board String)
    (define/private (setup)
      (let/ec done
        (let*-values ([(                lot) '()]
                      [(player1-worker1 lot) (placement one one-name 1 done lot)]
                      [(player2-worker1 lot) (placement two two-name 1 done lot)]
                      [(player1-worker2 lot) (placement one one-name 2 done lot)]
                      [(player2-worker2 lot) (placement two two-name 2 done lot)])
          (init player1-worker1 player1-worker2 player2-worker1 player2-worker2))))
    
    ;; (Player String [String -> Empty] Placements -> (values (List Worker [List N N]) Placements))
    ;; get a placement for a specific worker from target
    ;; EFFECT escape with done if target produces a location that is already occupied 
    (define/private (placement target target-name worker# done lot)
      (define new-place (send target placement lot))
      (when (memf (lambda (t) (equal? (rest t) new-place)) lot)
        (done (format BAD-PLACEMENT:fmt target-name)))
      (define full-name (string-append target-name (number->string worker#)))
      (values (cons (worker full-name) new-place) (cons (cons target-name new-place) lot)))

    ;; -----------------------------------------------------------------------------
    ;; Board ~~ the initial board, with four workers, two per player 
    (field [board (setup)])
    ;; -----------------------------------------------------------------------------

    ;; -> String
    ;; determine the winner (and the loser)
    (define/public (play)
      (if (string? board) board (play-rounds board)))

    (define/public (play-rounds board (one one) (one-name one-name) (two two) (two-name two-name))
      (define a (send one take-turn board))
      (displayln a)
      (displayln (apply-action board a))
      (cond
        [(not (check-action board a)) (format BAD-MOVE:fmt two-name one-name a)]
        [(giving-up? a) (format GIVING-UP:fmt two-name one-name)]
        [(winning-move? a) (format WINNING:fmt one-name)]
        [(move-build? a) (play-rounds (apply-action board a) two two-name one one-name)]))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))

  (define (make-mock-player% lot (tt void))
    (class object% (init-field name)
      (super-new)
      (define/public (other s) (void))
      (define/public (placement _lot) (begin0 (first lot) (set! lot (rest lot))))
      (define/public (take-turn board) (tt board))))

  (define-syntax checker
    (syntax-rules ()
      [(checker r action (args ...) lot tt ...)
       (check-equal?
        (parameterize ([current-output-port (open-output-string)])
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
  (checker (format BAD-MOVE:fmt "two" "one" bad-action) send (play) diagonal (lambda (b) bad-action))

  (define-board board-2-rounds-play
    [[2one1 2two1 3]
     [2one2 2two2 4]
     [2     4    ]
     [3     4    ]])
  board-2-rounds-play
  (define actions
    (list (move-build (worker "one2") PUT SOUTH PUT SOUTH) (winning-move (worker "two1") EAST PUT)))
  (define (stepper b) (begin0 (first actions) (set! actions (rest actions))))
  (checker (format WINNING:fmt "two") send (play-rounds board-2-rounds-play) diagonal stepper))

;; ---------------------------------------------------------------------------------------------------
(module+ main
  
  (define admin
    (new referee%
         [one (new player% [name "mf"])]
         [two (new player% [name "cd"])]))

  (send admin play))
