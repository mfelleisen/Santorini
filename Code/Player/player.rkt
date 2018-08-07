#lang racket

(provide
 player%)

;; ---------------------------------------------------------------------------------------------------
(require "../Common/board.rkt")

(define player%
  (class object% (init-field name)
    (super-new)

    (field [other-name ""])
    
    ;; String -> Void
    ;; inform this player of the name of the other player 
    (define/public (other name) (void))

    ;; [Listof (list N N)] -> Token
    ;; create a new token with distinct locations from the given ones
    (define/public (placement list-of-places)
      (cond
        [(empty? list-of-places) (list 0 0)]
        [else (define max-x (first (argmax first list-of-places)))
              (define max-y (first (argmax second list-of-places)))
              (list (+ 1 max-x) (+ 1 max-y))]))
      
    ;; Board ->
    ;;   (values) -- if the player gives up
    ;;   (values token east-west/c north-south/c) -- if the player claims a winning move
    ;;   (values token east-west/c north-south/c east-west/c north-south/c) ~~ move + build up
    (define/public (take-turn board)
      (values (token "x" 1 1) 0 0))))