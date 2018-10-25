#lang racket

;; provide a player that goes into an infinite loop during 1st placement
(require "failing.rkt")
(failing-module 1 #:p-failure (lambda (l) (let loop () (loop))))