#lang racket

;; The Referee plays a single game for two players.
;; The class is instanitated with two players. 

(require "referee-interface.rkt")

(provide
 (contract-out
  (admin% admin%/c)))

;; ---------------------------------------------------------------------------------------------------
(require "../Player/player.rkt")
(require "../Common/worker.rkt")
(require "../Common/board.rkt")
(require "../Common/actions.rkt")

(define admin%
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
                      [(player1-worker2 lot) (placement one one-name 2 done lot)]
                      [(player2-worker1 lot) (placement two two-name 1 done lot)]
                      [(player2-worker2 lot) (placement two two-name 2 done lot)])
          (init player1-worker1 player1-worker2 player2-worker1 player2-worker2))))

    ;; Placements = [Listof [List String N N]]

    #; (Player<%> String [String -> Empty] Placements -> (values (List Worker [List N N]) Placements))
    ;; get a placement for a specific worker from target
    ;; EFFECT escape with done if target produces a location that is already occupied 
    (define/private (placement target target-name worker# done lot)
      (define new-location (send target placement lot))
      (when (member new-location lot)
        (done (format "~a broke the rules of placing workers" target-name)))
      (define full-name (format "~a~a" target-name worker#))
      (values (cons (worker full-name) new-location) (cons (cons target-name new-location) lot)))
      
    (field [board (setup)])

    ;; -> String
    ;; determine the winner (and the loser)
    (define/public (play)
      (if (string? board) board (play-rounds board)))

    (define/private (report winner rule-breaker a)
      (format "~a\n~a broke the rules\n [~e]" winner rule-breaker a))

    (define/private (play-rounds board (one one) (one-name one-name) (two two) (two-name two-name))
      (define a (send one take-turn board))
      (displayln a)
      (displayln (apply-action board a))
      (if (not (check-action board a))
          (report two-name one-name a)
          (match a
            [(giving-up a)
             (format "~a, because ~a gave up" two-name one-name)]
            [(winning-move worker e-w-move n-s-move)
             (format "~a made a winning move" one-name)]
            [(move-build worker e-w-move n-s-move e-w-build n-s-build)
             ;; BUG: I forgot to build
             (play-rounds (apply-action board a) two two-name one one-name)])))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  (define admin
    (new admin%
         [one (new player% [name "mf"])]
         [two (new player% [name "cd"])]))

  (send admin play))
