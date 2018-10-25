#lang racket

;; provide a player that goes into an infinite loop after 2 correct turns
(require "failing.rkt")
(failing-module 2 #:tt-failure (lambda (board) (let loop () (loop))))