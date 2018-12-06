#lang racket

;; Create players that use the same strategy as player but fail after a certain number of rounds 

(require "../Common/player-interface.rkt")

(define step# (and/c natural-number/c (>=/c 0)))

(provide

 ;; SYNTAX
 ;; (failing-module n:N k:keyword f:function-for-failure)
 failing-module

 (contract-out 
  (make-failing-player%
   ;; n : number ~~ the constructed player will fail after n calls to other
   ;; n : (cons g t) ~~ ... will fail after g calls to other and t steps in the gam,
   ;;   including the 2 placement steps. 
   ;; depending on whether pf or ttf are supplied, the placement or take-turn method will fail 
   (->i ((n (or/c step# (cons/c step# step#))))
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
(define (make-failing-player% n* #:p-failure (pf #f) #:tt-failure (tff #f))
  (define n0 (if (number? n*) n* (car n*)))
  (define k0 (if (number? n*) 0  (cdr n*)))
  (class super%
    (super-new (other "aaaaaxxxx"))

    (inherit-field name strategy)

    (define games-in-series# n0)
    (define steps-in-game# k0)
    (define/augment (other-name oname)
      (set! games-in-series# (- games-in-series# 1))
      (set! strategy (new strategy% [player name][other oname])))

    (define-syntax-rule
      (define/failure (method arg) fail smethod)
      (define/override (method arg)
        (set! steps-in-game# (- steps-in-game# 1))
        (if (and (<= games-in-series# 0) (<= steps-in-game# 0) fail)
            (fail arg)
            (super method arg))))

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

  (define (mk-baddy keywords kw-args #:n (n 1))
    (define bad-player% (keyword-apply make-failing-player% keywords kw-args n '()))
    (define bad-player  (new bad-player% [name "matthias"]))
    (send bad-player other-name "christos")
    (send bad-player playing-as "matthiastwo")
    bad-player)

  (define bad1-plmnt (mk-baddy '(#:p-failure) `(,(λ (l) (if (empty? l) '(-1 0) (rest (first l)))))))  
  (check-equal? (send bad1-plmnt placement '(("christos" 0 0))) '(0 0))
  (check-exn exn:fail:contract? (lambda () (send bad1-plmnt placement '())))

  (define bad2-plmnt
    (mk-baddy '(#:p-failure) `(,(λ (l) (if (empty? l) '(-1 0) (rest (first l))))) #:n (cons 1 0)))
  (check-equal? (send bad2-plmnt placement '(("christos" 0 0))) '(0 0))
  (check-exn exn:fail:contract? (lambda () (send bad2-plmnt placement '())))

  (define board0 (cboard [[1matthias1 1matthias2] [1christos1 1christos2]]))

  (define bad2-tt (mk-baddy '(#:tt-failure) `(,(lambda (board) (/ 1 0)))))
  (check-exn exn:fail:contract:divide-by-zero? (lambda () (send bad2-tt take-turn board0)))

  (define bad1-tt (mk-baddy '(#:tt-failure) `(,(lambda (board) (/ 1 0))) #:n (cons 1 0)))
  (check-exn exn:fail:contract:divide-by-zero? (lambda () (send bad1-tt take-turn board0)))

  (define bad3-tt (mk-baddy '(#:tt-failure) `(,(lambda (board) (/ 1 0))) #:n (cons 1 1)))
  (check-exn exn:fail:contract:divide-by-zero?
             (lambda ()
               (with-handlers ((exn:fail:contract? void))
                 (send bad1-tt take-turn board0))
               (send bad1-tt take-turn board0))))
