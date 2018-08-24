#lang racket

(provide
 ;; a contract that describes the interface of a strategy class 
 strategy%/c

 ;; a contracts that describes the interface of a strategy object 
 strategy/c)

;; ---------------------------------------------------------------------------------------------------
(require "../Common/player-interface.rkt")
(module+ test
  (require (submod "../Common/board.rkt" test-support))
  (require rackunit))


;; ---------------------------------------------------------------------------------------------------
(define strategy%/c
  (class/c

   (init-field
    (player string?)
    (other  string?))
   
   (initialization
    ;; pick the next place for a worker
    ;; called twice in alternating ways, but 'other' made a placement in between 
    (->i ((this any/c) (lop  (this) (and/c placements/c (placement-protocol/c this)))) (r place/c)))
   
   (take-turn 
    ;;  pick the next action
    ;; called after initialization 
    (->i ((this any/c) (b board?)) (r action?)))))

(define strategy/c (instanceof/c strategy%/c))

(define ((placement-protocol/c this) placements)
  (define me (get-field player this))
  (cond
    ;; second call: 
    [(and (>= (string-length me) 2) (string=? (substring me 0 2) "1-"))
     (define old-me (substring me 2))
     (set-field! player this old-me)
     (define you (get-field other this))
     (unless (placed-at-least-one placements old-me)
       (displayln `(during the second call *I* must have placed one worker) (current-error-port)))
     (unless (placed-at-least-one placements you)
       (displayln `(during the second call *you* must have placed one worker) (current-error-port)))
     (and (placed-at-least-one placements old-me)
          (placed-at-least-one placements you))]
    ;; first call: 
    [else
     (set-field! player this (string-append "1-" me))
     (when (placed-at-least-one placements me)
       (displayln `(during the first call *I* must not have placed a worker) (current-error-port)))
     (not (placed-at-least-one placements me))]))

(define (placed-at-least-one placements you)
  (for/first ((p placements) #:when (string=? (first p) you)) p))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-false  (placed-at-least-one '() "cd"))
  (check-equal? (placed-at-least-one `(("cd" 0 0)) "cd") '("cd" 0 0))

  ;; -------------------------------------------------------------------------------------------------
  (define/contract mock-strategy% strategy%/c
    (class object% (init-field player other)
      (super-new)
      (define/public (initialization lop) (list 0 0))
      (define/public (take-turn b) (giving-up player))))

  (check-pred cons? (send (new mock-strategy% [player "x"][other "o"]) initialization '()) "test in")

  (define b (cboard [[0x1 0x2][0o1 0o2]]))
  (check-pred giving-up? (send (new mock-strategy% [player "x"][other "o"]) take-turn b) "test tt")

  (define (protocoled-mocked-strategy)
    (define mock-strategy (new mock-strategy% [player "x"][other "o"]))
    (placement-protocol/c mock-strategy))
  
  (check-true   ((protocoled-mocked-strategy) '()))
  (check-equal? (let ([s (protocoled-mocked-strategy)])
                  (s '()) ;; first, irrelevant call 
                  (s '(("o" 1 1) ("x" 1 2))))
                '("o" 1 1)))
