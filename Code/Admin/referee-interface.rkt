#lang racket

(provide
 admin%/c)

;; -----------------------------------------------------------------------------
(require "../Common/player-interface.rkt")
;; -----------------------------------------------------------------------------
(define admin%/c
   (class/c
    (init-field (one player/c) (two player/c))
    (play (->m string?))))