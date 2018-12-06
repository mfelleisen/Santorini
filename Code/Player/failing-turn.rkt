#lang racket

;; provide a player that fails to play a correct first in its second game
(require "failing.rkt")
(failing-module 2 #:tt-failure (lambda (board) (/ 1 0)))