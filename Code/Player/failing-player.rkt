#lang racket

(require "../Common/player-interface.rkt")

(provide
 (contract-out 
  (make-failing-player%
   (->i ((n (and natural-number/c positive?)))
        (#:p-failure (pf void)
         #:tt-failure (tff void))
        #:pre/name (pf tff) "exactly one of them is not specified"
        (and (or (unsupplied-arg? pf) (unsupplied-arg? tff)) (not (eq? pf tff)))
        (result player%/c)))))
 
;; ---------------------------------------------------------------------------------------------------
(require "../Common/board.rkt")
(require "../Common/actions.rkt")
(require "strategy.rkt")

(module+ test
  (require (submod "../Common/board.rkt" test))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (make-failing-player% n #:p-failure (pf void) #:tt-failure (tff void))
  (class object% (init-field name)
    (super-new)

    (define other-name "")
    (define strategy #f)
    
    (define/public (other other-name)
      (set! n (- n 1))
      (set! other-name other-name)
      (set! strategy (new strategy% [player name][other other-name])))
    
    (define/public (placement list-of-places)
      (if (= n 0)
          (pf list-of-places)
          (send strategy initialization list-of-places)))
    
    (define/public (take-turn board)
      (if (= n 0)
          (tff board)
          (send strategy take-turn board)))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))

  (check-exn #px"exactly one of them is not specified"
             (lambda () (make-failing-player% 1 #:p-failure list #:tt-failure cons)))

  (define (mk-baddy keywords kw-args)
    (define bad-player% (keyword-apply make-failing-player% keywords kw-args 1 '()))
    (define bad-player  (new bad-player% [name "matthias"]))
    (send bad-player other "christos")
    bad-player)

  (define baddy-p (mk-baddy '(#:p-failure) `(,(λ (l) (if (empty? l) '(-1 0) (rest (first l)))))))  
  (check-equal? (send baddy-p placement '(("christos" 0 0))) '(0 0))
  (check-exn exn:fail:contract? (lambda () (send baddy-p placement '())))

  (define baddy-take-turn (mk-baddy '(#:tt-failure) `(,(lambda (board) (/ 1 0)))))
  (define board0 (cboard [[1matthias1 1matthias2] [1christos1 1christos2]]))
  (check-exn exn:fail:contract:divide-by-zero? (lambda () (send baddy-take-turn take-turn board0))))