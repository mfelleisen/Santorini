#lang racket

;; provide a player that goes into an infinite loop during the first turn of the first game
(require "failing.rkt")
(define inf-tt (lambda (board) (let loop () (loop))))
(failing-module 1 #:tt-failure inf-tt)