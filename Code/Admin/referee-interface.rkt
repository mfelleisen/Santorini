#lang racket


(provide
 ;; type Terminated = (terminated String String)
 terminated
 
 terminated?
 
 referee%/c)

;; -----------------------------------------------------------------------------
(require "../Common/player-interface.rkt")
;; -----------------------------------------------------------------------------

(struct terminated [winner message] #:transparent)

(define referee%/c
   (class/c

    (init-field
     (one player/c)
     (two player/c))
    
    (best-of (->m (and/c natural-number/c odd?) (or/c string? terminated?)))
    (play    (->m (or/c string? terminated?)))))

