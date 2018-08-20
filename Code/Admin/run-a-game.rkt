#lang racket

(require "referee.rkt")
(require "../Player/player.rkt")
(require "../Player/strategy.rkt")
(require "../Lib/xsend.rkt")

(define referee
  (new referee%
       [one (new player% [name "mf"])]
       [two (new player% [name "cd"])]))

(module+ main 
  (time-out-limit 1.2)
  (send referee best-of 3))
;; ---------------------------------------------------------------------------------------------------
