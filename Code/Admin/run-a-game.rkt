#lang racket

;; ---------------------------------------------------------------------------------------------------
(require "referee.rkt")
(require "../Player/player.rkt")
(require "../Observer/textual.rkt")

(module+ main
  (require "../Lib/xsend.rkt"))

;; ---------------------------------------------------------------------------------------------------
(module+ main
  (time-out-limit 1.2)
  (define observer (new textual-observer%))
  (define player1  (new player% [name "mf"]))
  (define player2  (new player% [name "cd"]))
  (define referee  (new referee% [one player1] [one-name "mf"] [two player2] [two-name "cd"]))
  (send referee register observer)
  (send referee play)
  #;
  (send referee best-of 3))

