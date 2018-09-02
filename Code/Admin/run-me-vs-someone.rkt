#lang racket

;; ---------------------------------------------------------------------------------------------------
(require "referee.rkt")
(require "../Player/player.rkt")
(require "../Player/textual.rkt")

(require "../Lib/xsend.rkt")

;; ---------------------------------------------------------------------------------------------------

(define matthias (new textual% [name "matthias"]))

(time-out-limit 200) ;; this is interactive

(define referee
  (new referee%
       [one (new player%  [name "autochris"])]
       [two matthias]))

(module+ main 
  (send referee play))

