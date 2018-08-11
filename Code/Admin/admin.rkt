#lang racket

;; The Referee plays a single game for two players.
;; The class is instanitated with two players. 

(define player/c
  (object/c
   (init-field name)
   (placement (->m (listof (list/c in-range? in-range?)) (list/c in-range? in-range?)))
   (take-turn (->m board? action?))))
   

(provide
 (contract-out
  (admin%
   (class/c
    (init-field (one player/c) (two player/c))
    (play (->m string?))))))


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

    #; (Player<%> String [String -> Empty] [Listof [List N N]] ->
                  (values (List Worker [List N N]) [Listof [List N N]]))
    ;; get a placement for a specific worker from target
    ;; EFFECT escape with done if target produces a location that is already occupied 
    (define/private (placement target target-name worker# done lot)
      (define new-location (send target placement lot))
      (when (member new-location lot)
        (done (format "~a broke the rules of placing workers" target-name)))
      (define full-name (format "~a~a" target-name worker#))
      (values (cons (worker full-name) new-location) (cons new-location lot)))
      
    (field [board (setup)])

    ;; -> String
    ;; determine the winner (and the loser)
    (define/public (play)
      (if (string? board) board (play-rounds board)))

    (define/private (report winner rule-breaker a)
      (format "~a\n~a broke the rules\n [~e]" winner rule-breaker a))

    (define/private (play-rounds board (one one) (two two))
      (define a (send one take-turn board))
      (displayln (apply-action board a))
      (if (not (check-action board a))
          (report (get-field name two) (get-field name one) a)
          (match a
            [(giving-up)
             (format "~a, because ~a gave up" (get-field name two) (get-field name one))]
            [(winning-move worker e-w-move n-s-move)
             (format "~a made a winning move" (get-field name one))]
            [(move-build worker e-w-move n-s-move e-w-build n-s-build)
             ;; BUG: I forgot to build
             (play-rounds (apply-action board a) two one)])))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  (define admin
    (new admin%
         [one (new player% [name "mf"])]
         [two (new player% [name "cd"])]))

  (send admin play))
