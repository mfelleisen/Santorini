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

   (init-field
    (player string?)
    (other  string?))
   
   (initialization
    ;; pick the next place for a worker
    ;; called twice in alternating ways, but 'other' made a placement in between 
    (->i ((this any/c)
          (lop  (this) (and/c placements/c
                              (lambda (placements)
                                (define me (get-field player this))
                                (cond
                                  ;; second call: 
                                  [(and (>= (string-length me) 2) (string=? (substring me 0 2) "1-"))
                                   (define old-me (substring me 2))
                                   (set-field! player this old-me)
                                   (define you (get-field other this))
                                   (and (placed-at-least-one placements old-me)
                                        (placed-at-least-one placements you))]
                                  ;; first call: 
                                  [else
                                   (set-field! player this (string-append "1-" me))
                                   (not (placed-at-least-one placements me))])))))
         (r place/c)))
   
   (take-turn 
    ;;  pick the next action
    ;; called after initialization 
    (->i ((this any/c) (b board?)) (r action?)))))

(define strategy/c (instanceof/c strategy%/c))

(define (tee x) (displayln x) x)

;; Strategy N -> [Placements -> (U Boolean APlacement)]

(define (placed-at-least-one placements you)
  (for/first ((p placements) #:when (string=? (first p) you)) p))



(module+ test
  (require rackunit)
  (check-false  (placed-at-least-one '() "cd"))
  (check-equal? (placed-at-least-one `(("cd" 0 0)) "cd") '("cd" 0 0)))