#lang racket

;; ---------------------------------------------------------------------------------------------------
(require "referee.rkt")
(require "../Player/player.rkt")
(require "../Player/textual.rkt")

(require "../Lib/xsend.rkt")

;; ---------------------------------------------------------------------------------------------------

(time-out-limit 200) ;; this is interactive

(module+ main
  (define matthias  (new textual% [name "machine"]))
  (define autochris (new player%  [name "chrisf"]))
  (define referee   (new referee% [one autochris] [two matthias]))
  (send referee play))

