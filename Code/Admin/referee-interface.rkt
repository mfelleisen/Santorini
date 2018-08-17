#lang racket

(provide
 referee%/c)

;; -----------------------------------------------------------------------------
(require "../Common/player-interface.rkt")
;; -----------------------------------------------------------------------------
(define referee%/c
   (class/c

    (init-field
     (one player/c)
     (two player/c))
    
    (best-of (->m (and/c natural-number/c odd?) string?))
    (play    (->m string?))))