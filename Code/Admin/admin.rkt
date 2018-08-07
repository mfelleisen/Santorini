#lang racket

;; ---------------------------------------------------------------------------------------------------
(require "rule-checking.rkt")
(require "../Common/board.rkt")

(define admin%
  (class object% (init-field one two)
    (super-new)

    ;; -- set up --- 

    (field
     [one-name (get-field name one)]
     [two-name (get-field name two)])

    (send one other two-name)
    (send two other one-name)

    (define/private (placement target name lot)
      (define new-location (send target placement lot))
      (and (not (member new-location lot)) new-location))

    ;; -> (U String [Listof Token])
    (define/private (setup)
      (let/ec done
        (define-values (_ tokens)
          (for/fold ([place* '()][token* '()]) ([n '(1 2 3 4)])
            (define-values (player name)
              (if (odd? n) (values one one-name) (values two two-name)))
            (define new-place (placement player name place*))
            (if (boolean? new-place)
                (done (format "~a broke the rules about placing workers" name))
                (values (cons new-place place*) (cons (apply token name new-place) token*)))))
        (apply init tokens)))

    (field [board (setup)])

    (define/private (play-rounds board (one one) (two two))
      (call-with-values
       (lambda () (send one take-turn board))
       (case-lambda
         [() (get-field name two)]
         [(token e-w-move n-s-move)
          (if (and (check-move board token e-w-move n-s-move) (is-token-a-winner? board token))
              (get-field name one)
              (get-field name two))]
         [(token e-w-move n-s-move e-w-build n-s-build)
          (cond
            [(not (check-move board token e-w-move n-s-move)) (get-field name two)]
            [else (define new-token (move-token token e-w-move n-s-move))
                  (define new-board (move board token e-w-move n-s-move))
                  (if (not (check-build-up new-board new-token e-w-build n-s-build))
                      (get-field name two)
                      (play-rounds new-board two one))])])))
                                 
    ;; -> (U String)
    ;; determine the winner (and the loser)
    (define/public (play)
      (if (string? board)
          board
          (play-rounds board)))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define admin
    (new admin%
         [one (new player% [name "mf"])]
         [two (new player% [name "cd"])]))

  (send admin play))