#lang racket

;; Create players that use the same strategy as player but fail after a certain number of rounds 

(require "../Common/player-interface.rkt")

(provide

 ;; SYNTAX
 ;; (failing-module n:N k:keyword f:function-for-failure)
 failing-module

 (contract-out 
  (make-failing-player%
   ;; the constructed player will fail after n calls to other
   ;; depending on whether pf or ttf are supplied, the placement or take-turn method will fail 
   (->i ((n (and natural-number/c positive?)))
        (#:p-failure  (pf  (-> placements/c place/c))
         #:tt-failure (tff (-> board? action?)))
        #:pre/name (pf tff) "exactly one of them is not specified"
        (and (or (unsupplied-arg? pf) (unsupplied-arg? tff)) (not (eq? pf tff)))
        (result player-protocol%/c)))))
 
;; ---------------------------------------------------------------------------------------------------
(require "super.rkt")
(require "strategy.rkt")

(module+ test
  (require (submod "../Common/board.rkt" test-support))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (make-failing-player% n0 #:p-failure (pf #f) #:tt-failure (tff #f))
  (class super%
    (super-new (other "aaaaaxxxx"))

    (inherit-field name strategy)

    (define n n0)
    (define/augment (other-name oname)
      (set! n (- n 1))
      (set! strategy (new strategy% [player name][other oname])))

    (define-syntax-rule
      (define/failure (method arg) fail smethod)
      (define/override (method arg) (if (and (<= n 0) fail) (fail arg) (super method arg))))

    (define/failure (placement list-of-places) pf initialization)

    (define/failure (take-turn board) tff take-turn)))

(define-syntax-rule (failing-module n kw f)
  (begin

    (require "../Common/player-interface.rkt")

    (provide
     (contract-out
      (rename
       failing-after-3-for-take-turn%
       player%
       player%/c)))

    ;; -----------------------------------------------------------------------------------------------
    (require "failing.rkt")

    (define failing-after-3-for-take-turn%
      (make-failing-player% n kw f))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))

  (check-exn #px"exactly one of them is not specified"
             (lambda () (make-failing-player% 1 #:p-failure list #:tt-failure cons)))

  (define (mk-baddy keywords kw-args)
    (define bad-player% (keyword-apply make-failing-player% keywords kw-args 1 '()))
    (define bad-player  (new bad-player% [name "matthias"]))
    (send bad-player other-name "christos")
    (send bad-player playing-as "matthias2")
    bad-player)

  (define baddy-p (mk-baddy '(#:p-failure) `(,(λ (l) (if (empty? l) '(-1 0) (rest (first l)))))))  
  (check-equal? (send baddy-p placement '(("christos" 0 0))) '(0 0))
  (check-exn exn:fail:contract? (lambda () (send baddy-p placement '())))

  (define baddy-take-turn (mk-baddy '(#:tt-failure) `(,(lambda (board) (/ 1 0)))))
  (define board0 (cboard [[1matthias1 1matthias2] [1christos1 1christos2]]))
  (check-exn exn:fail:contract:divide-by-zero? (lambda () (send baddy-take-turn take-turn board0))))
