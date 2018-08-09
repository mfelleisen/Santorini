#lang racket

;; ---------------------------------------------------------------------------------------------------
(require "../Common/rule-checking.rkt")
(require "../Player/player.rkt")
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

    (define/private (placement target lot)
      (define new-location (send target placement lot))
      (and (not (member new-location lot)) new-location))

    ;; -> (U String [Listof Token])
    (define/private (setup)
      (let/ec done
        (define-values (_ tokens)
          (for/fold ([place* '()][token* '()]) ([n '(1 2 3 4)])
            (define-values (player name) (if (odd? n) (values one one-name) (values two two-name)))
            (define new-place (placement player place*))
            (if (boolean? new-place)
                (done (format "~a broke the rules of placing workers" name))
                (values (cons new-place place*) (cons (apply token name new-place) token*)))))
        (apply init tokens)))

    (field [board (setup)])

    ;; -> (U String)
    ;; determine the winner (and the loser)
    (define/public (play)
      (if (string? board) board (play-rounds board)))

    (define/private (report winner rule-breaker a)
      (format "~a\n~a broke the rules\n [~e]" winner rule-breaker a))

    (define/private (play-rounds board (one one) (two two))
      (define a (send one take-turn board))
      (displayln a)
      (if (not (check-action board a))
          (report (get-field name two) (get-field name one) a)
          (match a
            [(giving-up)
             (format "~a, because ~a gave up" (get-field name two) (get-field name one))]
            [(winning-move token e-w-move n-s-move)
             (format "~a made a winning move" (get-field name one))]
            [(move-build token e-w-move n-s-move e-w-build n-s-build)
             ;; BUG: I forgot to build
             (play-rounds (apply-action board a) two one)])))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define admin
    (new admin%
         [one (new player% [name "mf"])]
         [two (new player% [name "cd"])]))

  (send admin play))