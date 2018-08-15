#lang racket

;; A player may ask the referee to perform an action on its behalf. There are three
;; kinds of actions: giving up, moving a worker to win, and a move followed by construction. 

;; ---------------------------------------------------------------------------------------------------

(require "../Lib/require.rkt")

(provide
 ;; type Action
 action?
 giving-up?    
 winning-move? 
 move-build?   

 (contract-out
  (giving-up    (-> string? action?))
  (winning-move (-> worker? east-west/c north-south/c action?))
  (move-build   (-> worker? east-west/c north-south/c east-west/c north-south/c action?))

  (apply-action
   ;; execute the given action on this board
   ;; ASSUME check-action has been called (too expansive to call again)
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
(require "../Lib/struct-with.rkt")


(module+ test
  (require (submod "board.rkt" test))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------

(struct action () #:transparent)
;; Action is one of:

;; -- *
(struct giving-up action (actor)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc b op mode) (write-giving-up b op mode))])

;; -- * 
(struct winning-move action (actor e-w-move n-s-move)
;;      [winning-move Worker EWDIR NSDIR]
;;                     t moves e-w & n-s and thus arrives at level 3 
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc b op mode) (write-winning b op mode))])

;; -- * 
(struct move-build action (actor e-w-move n-s-move e-w-build n-s-build)
;;      [move-build Worker EWDIR NSDIR EWDIR NSDIR]
;;                     t moves e-w & n-s, then builds in the specified directions
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc b op mode) (write-move-build b op mode))])

(define (apply-action board a)
  (match a
    [(giving-up a) board]
    [(winning-move t e-w-move n-s-move)
     (move board t  e-w-move n-s-move)]
    [(move-build t e-w-move n-s-move e-w-build n-s-build)
     (define new-board (move board t e-w-move n-s-move))
     (build new-board t e-w-build n-s-build)]))

(define (check-action board a)
  (match a
    [(giving-up a) #true] ;; players can give up for all kinds of reasons 
    [(winning-move t e-w n-s)
     (and (not (and (= e-w PUT) (= n-s PUT)))
          (stay-on-board? board t e-w n-s)
          (check-move board t e-w n-s)
          (is-move-a-winner? board t e-w n-s))]
    [(move-build t e-w n-s e-w-build n-s-build)
     (and (not (and (= e-w PUT) (= n-s PUT)))
          (stay-on-board? board t e-w n-s) 
          (not (and (= e-w-build PUT) (= n-s-build PUT)))
          (stay-on-board? (move board t e-w n-s) t e-w-build n-s-build)
          (check-build-up board t e-w n-s e-w-build n-s-build))]))

;; ---------------------------------------------------------------------------------------------------
;; priniting auxiliaries 

;; Giving-up OutputPort Boolean? -> Void 
(define (write-giving-up b op mode)
  (with giving-up b
        (define name actor)
        (display `(,name is giving up) op)))

;; Winning-move OutputPort Boolean? -> Void 
(define (write-winning b op mode)
  (with winning-move b
        (define name (worker-name actor))
        (define ews (e-w->string e-w-move))
        (define nss (n-s->string n-s-move))
        (display (append (move-pattern name ews nss) winner) op)))

;; Move-build OutputPort Boolean? -> Void 
(define (write-move-build b op mode)
  (with move-build b
        (define name (worker-name actor))
        (define ews (e-w->string e-w-move))
        (define nss (n-s->string n-s-move))
        (define bews (e-w->string e-w-build))
        (define bnss (n-s->string n-s-build))
        (display (append (move-pattern name ews nss) (mb-pattern bews bnss)) op)))

;; String String String -> S-expression 
(define (move-pattern name ews nss)
  `(,name requests to move ,ews and ,nss))

(define (mb-pattern bews bnss)
  `(and then to build to its ,bews and ,bnss))

(define winner '(and claims this move is a winner))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))

  (define-syntax-rule (check-apply b a r) (check-equal? (apply-action b a) r))
  (define-syntax-rule (check-check b a r) (check-equal? (check-action b a) r))

  (define (make-board ss0 tt)
    (define ss (if (number? ss0) ss0 (string->symbol (string-append (symbol->string ss0) "2"))))
    (define-board b
      [[1x1 2o1 4]
       [2x2 ,ss 4]
       [4   4   ,tt]])
    b)

  (define t1 (worker "o2"))

  (define t1-gu (giving-up t1))
  (define t1-wm (winning-move t1 EAST SOUTH))
  (define t1-mb (move-build t1 EAST SOUTH WEST NORTH))

  (check-apply (make-board '2o 4) t1-gu (make-board '2o 4))
  (check-apply (make-board '2o 3) t1-wm (make-board 2 '3o2))
  (check-apply (make-board '2o 2) t1-mb (make-board 3 '2o2))
  
  (check-check (make-board '2o 4) (giving-up t1) #t)
  (check-check (make-board '2o 3) (winning-move t1 EAST SOUTH) #t)
  (check-check (make-board '2o 2) (move-build t1 EAST SOUTH WEST NORTH) #t)
  (check-check (make-board '2o 3) (winning-move t1 WEST PUT) #f)
  (check-check (make-board '2o 2) (move-build t1 EAST SOUTH PUT NORTH) #f)

  ;; bug ?
  (define-board bug1-board
    [[0mf1 ]
     [0    0   0mf2]
     [0    0   0cd1 1  ]
     [0    0   0    0cd2]])

  (define-board bug1-expected
    [[0mf1 ]
     [0    0   0mf2]
     [0    0   0cd1 1   ]
     [0    0   0    0   ]
     [0    0   0    0   0cd2]
     [0    0   0    0   0    1]])
  
  (check-apply bug1-board (move-build (worker "cd2") 1 1 1 1) bug1-expected))

(module+ test
  

  (define-syntax-rule
    (check-out a r) (check-equal? (with-output-to-string (lambda () (displayln a))) r))

  (check-out t1-gu "(o is giving up)\n")
  (check-out t1-wm "(o requests to move EAST and SOUTH and claims this move is a winner)\n")
  (check-out t1-mb "(o requests to move EAST and SOUTH and then to build to its WEST and NORTH)\n"))
  
