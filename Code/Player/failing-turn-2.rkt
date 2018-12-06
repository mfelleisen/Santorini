#lang racket

;; provide a player that fails to play a correct first in its second game
(require "failing.rkt")
(require "../Common/board.rkt")
(require "../Common/actions.rkt")
(failing-module
 (cons 1 4)
 #:tt-failure
 (lambda (board)
   (define players (board-players board))
   (define workers (named-workers (first players)))
   (move-build (first workers) PUT PUT PUT PUT)))