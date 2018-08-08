#lang racket

(provide
 ;; type Action
 action?
 giving-up giving-up? 
 winning-move
 move-build

 (contract-out 
  (apply-action
   (->i ([b board?] [a action?]) (r board?)))))

;; ---------------------------------------------------------------------------------------------------
(require "board.rkt")

(module+ test
  (require (submod "board.rkt" test))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct action () #:transparent)
;; Action is one of:
(struct giving-up action () #:transparent)
(struct winning-move action (actor e-w-move n-s-move) #:transparent)
;;      [winning-move Token EWDIR NSDIR]
;;                     t moves e-w & n-s and thus arrives at level 3 
(struct move-build action (actor e-w-move n-s-move e-w-build n-s-build) #:transparent)
;;      [move-build Token EWDIR NSDIR EWDIR NSDIR]
;;                     t moves e-w & n-s, then builds in the specified directions

;; Board Action -> Board 
(define (apply-action board a)
  (match a
    [(giving-up) board]
    [(winning-move t e-w-move n-s-move)
     (move board t  e-w-move n-s-move)]
    [(move-build t e-w-move n-s-move e-w-build n-s-build)
     (define new-token (move-token t e-w-move n-s-move))
     (define new-board (move board t e-w-move n-s-move))
     (build new-board new-token e-w-build n-s-build)]))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))

  (define (make-board ss tt)
    (define-board b
      [[1x 2o  4]
       [2x ,ss 4]
       [4  4   ,tt]])
    b)

  (check-equal? (apply-action
                 (make-board (list 2 "o") 4)
                 (giving-up))
                (make-board (list 2 "o") 4))

  (check-equal? (apply-action
                 (make-board (list 2 "o") 3)
                 (winning-move (token "o" 1 1) EAST SOUTH))
                (make-board 2 (list 3 "o")))

  (check-equal? (apply-action
                 (make-board (list 2 "o") 2)
                 (move-build (token "o" 1 1) EAST SOUTH WEST NORTH))
                (make-board 3 (list 2 "o")))
                
  )