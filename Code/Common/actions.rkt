#lang racket

(provide
 ;; type Action
 action?
 giving-up giving-up? 
 winning-move winning-move? 
 move-build

 (contract-out 
  (apply-action
   ;; execute the given action on this board
   (->i ([b board?] [a action?]) (r board?)))

  (check-action
   ;; is the given action legal on this board? 
   (-> board? action? boolean?))))

;; ---------------------------------------------------------------------------------------------------
(require "board.rkt")
(require "rule-checking.rkt")
(require "worker.rkt")
(require "directions.rkt")
(require "buildings.rkt")
(require "../Lib/require.rkt")

(module+ test
  (require (submod "board.rkt" test))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct action () #:transparent)
;; Action is one of:
(struct giving-up action () #:transparent)
(struct winning-move action (actor e-w-move n-s-move) #:transparent)
;;      [winning-move Worker EWDIR NSDIR]
;;                     t moves e-w & n-s and thus arrives at level 3 
(struct move-build action (actor e-w-move n-s-move e-w-build n-s-build) #:transparent)
;;      [move-build Worker EWDIR NSDIR EWDIR NSDIR]
;;                     t moves e-w & n-s, then builds in the specified directions

(define (apply-action board a)
  (match a
    [(giving-up) board]
    [(winning-move t e-w-move n-s-move)
     (move board t  e-w-move n-s-move)]
    [(move-build t e-w-move n-s-move e-w-build n-s-build)
     (define new-board (move board t e-w-move n-s-move))
     (build new-board t e-w-build n-s-build)]))

(define (check-action board a)
  (match a
    [(giving-up) #true] ;; players can give up for all kinds of reasons 
    [(winning-move t e-w n-s)
     (and (check-move board t e-w n-s) (is-move-a-winner? board t e-w n-s))]
    [(move-build t e-w n-s e-w-build n-s-build)
     (and (check-move board t e-w n-s)
          (check-build-up board t e-w n-s e-w-build n-s-build))]))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))

  (define-syntax-rule (check-apply b a r) (check-equal? (apply-action b a) r))
  (define-syntax-rule (check-check b a r) (check-equal? (check-action b a) r))

  (define (make-board ss0 tt)
    (define ss (if (number? ss0) ss0 (string->symbol (string-append (symbol->string ss0) "2"))))
    (define-board b
      [[1x1 2o1 4]
       [2x1 ,ss 4]
       [4   4   ,tt]])
    b)

  (define t1 (worker "o2"))

  (check-apply (make-board '2o 4) (giving-up) (make-board '2o 4))
  (check-apply (make-board '2o 3) (winning-move t1 EAST SOUTH) (make-board 2 '3o2))
  (check-apply (make-board '2o 2) (move-build t1 EAST SOUTH WEST NORTH) (make-board 3 '2o2))
  
  (check-check (make-board '2o 4) (giving-up) #t)
  (check-check (make-board '2o 3) (winning-move t1 EAST SOUTH) #t)
  (check-check (make-board '2o 2) (move-build t1 EAST SOUTH WEST NORTH) #t)
  (check-check (make-board '2o 3) (winning-move t1 WEST PUT) #f)
  (check-check (make-board '2o 2) (move-build t1 EAST SOUTH PUT NORTH) #f))
