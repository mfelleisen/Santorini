#lang racket

;; provide a player that goes into an infinite loop after 2 correct turns
(require "failing.rkt")
(define inf-tt (lambda (board) (let loop () (loop))))
(failing-module 1 #:tt-failure inf-tt)