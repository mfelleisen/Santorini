#lang racket

;; what knowledge is turned into information here and represented with data:
;; -- a worker represents a position on the board 
;; -- a direction tells me where the worker goes 
;; -- .. or direction a worker eyes for adding a level to a building 

;; ---------------------------------------------------------------------------------------------------

(define PLAYER-NAME #px"^[a-z]+$")
(define WORKER-NAME #px"^([a-z]+)[1,2]")

(provide
 (contract-out
  (good-player-name? (-> any/c boolean?))
  (good-worker-name? (-> any/c boolean?)))

 ;; type Worker = (worker String Range Range)
 worker?
 (contract-out
  (worker       (-> (flat-named-contract "properly named worker" good-worker-name?) worker?))
  (worker-name+no (-> worker? string?))
  (worker-name  (-> worker? string?))))

;; ---------------------------------------------------------------------------------------------------
(require "../Lib/struct-with.rkt")
(module+ test (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct worker (name+no) #:transparent)

(define (worker-name w)
  (with worker w (second (regexp-match WORKER-NAME name+no))))

(module+ test
  (require (submod ".."))
  (check-exn #px"properly named worker" (lambda () (worker "baddy-1")))
  (check-equal? (worker-name (worker "x1")) "x"))


(define (good-player-name? name)
  (and (string? name) (regexp-match PLAYER-NAME name) #t))

(define (good-worker-name? name)
  (and (string? name) (regexp-match WORKER-NAME name) #t))

(module+ test
  (require rackunit)

  (check-true (good-player-name? "matthias"))
  (check-false (good-player-name? "matthias1"))
  (check-false (good-player-name? "0matthias"))
  (check-false (good-player-name? ""))

  (check-true (good-worker-name? "matthias1"))
  (check-false (good-worker-name? "0matthias1"))
  (check-false (good-worker-name? "")))