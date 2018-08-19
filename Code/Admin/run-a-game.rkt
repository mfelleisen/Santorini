#lang racket

(require "referee.rkt")
(require "../Player/player.rkt")
(require "../Lib/xsend.rkt")

(define admin
  (new referee%
       [one (new player% [name "mf"])]
       [two (new player% [name "cd"])]))

(time-out-limit 1.2)
(send admin best-of 3)