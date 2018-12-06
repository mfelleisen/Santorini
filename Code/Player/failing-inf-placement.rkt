#lang racket

;; provide a player that goes into an infinite loop during 1st placement of 1st game 
(require "failing.rkt")
(define inf-pl (lambda (l) (let loop () (loop))))
(failing-module 1 #:p-failure inf-pl)