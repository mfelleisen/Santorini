#lang racket

(provide
 ;; type Action
 action?
 action

 apply-action

  #;
  (action
   (-> token? east-west/c north-south/c east-west/c north-south/c action?)))

;; ---------------------------------------------------------------------------------------------------
(require "board.rkt")

(struct action (actor e-w-move n-s-move e-w-build n-s-build) #:transparent)
;; Action   = [action Token EWDIR NSDIR EWDIR NSDIR]
;;                     t moves e-w & n-s, then builds in the specified directions
;;          | [action Token EWDIR NSDIR]
;;                     t moves e-w & n-s and thus arrives at level 3 

;; Board Action -> Board 
(define (apply-action board a)
  (match-define (action t e-w-move n-s-move e-w-build n-s-build) a)
  (define new-token (move-token t e-w-move n-s-move))
  (define new-board (move board t e-w-move n-s-move))
  (build new-board new-token e-w-build n-s-build))