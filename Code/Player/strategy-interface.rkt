#lang racket

(provide
 ;; a contract that describes the interface of a strategy class 
 strategy%/c

 ;; a contracts that describes the interface of a strategy object 
 strategy/c)

;; ---------------------------------------------------------------------------------------------------
(require "../Common/board.rkt")
(require "../Common/actions.rkt")
(require "../Common/player-interface.rkt")

;; ---------------------------------------------------------------------------------------------------
(define (name/c b) (and/c string? (on? b)))

(define strategy%/c
  (class/c
   (init-field (player string?) (other string?))
   (initialization
    ;; pick the next place for a worker
    ;; called twice 
    (->i ((this any/c) (l placements/c)) (r place/c)))
   
   (take-turn 
    ;;  pick the next action
    ;; called after initialization 
    (->i ((this any/c) (b board?)) (r action?)))))

(define strategy/c (instanceof/c strategy%/c))