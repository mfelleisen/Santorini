#lang racket

;; provide a player that fails after 2 correct turns
(require "failing.rkt")
(failing-module 2 #:tt-failure (lambda (board) (/ 1 0)))