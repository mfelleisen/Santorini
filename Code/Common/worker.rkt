#lang racket

;; what knowledge is turned into information here and represented with data:
;; -- a worker represents a position on the board 
;; -- a direction tells me where the worker goes 
;; -- .. or direction a worker eyes for adding a level to a building 

;; ---------------------------------------------------------------------------------------------------

(define NAME #px"^([a-z]+)[1,2]")

(provide
 ;; type Worker = (worker String Range Range)
 worker?
 (contract-out
  (worker       (-> (flat-named-contract "properly named worker" (curry regexp-match NAME)) worker?))
  (worker-name+no (-> worker? string?))
  (worker-name  (-> worker? string?))))

;; ---------------------------------------------------------------------------------------------------
(require "directions.rkt")
(require "../Lib/struct-with.rkt")
(module+ test (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct worker (name+no) #:transparent)

(define (worker-name w)
  (with worker w (second (regexp-match NAME name+no))))

(module+ test
  (require (submod ".."))
  (check-exn #px"properly named worker" (lambda () (worker "baddy-1")))
  (check-equal? (worker-name (worker "x1")) "x"))