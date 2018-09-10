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
  (define referee (new referee% [one (new player% [name "mf"])] [two (new player% [name "cd"])]))
  (send referee register observer)
  (send referee play)
  #;
  (send referee best-of 3))

