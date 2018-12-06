#lang racket

;; provide a player that fails to place workers correctly during 1st placement in its first game
(require "failing.rkt")
(failing-module 1 #:p-failure (lambda (l) (if (empty? l) '(0 -1) (caar l))))