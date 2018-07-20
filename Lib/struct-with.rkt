#lang racket

;; -----------------------------------------------------------------------------
(provide
 ;; SYNTAX
 ;; (struct-with name (field ...) option ...)
 ;; defines
 ;; (1) a (struct name (field ...) option ...)
 ;; (2) a (with-name n:expr element ...) syntax for destructuring a struct name
 struct-with)

;; -----------------------------------------------------------------------------
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

;; -----------------------------------------------------------------------------
(define-syntax (struct-with stx)
  (syntax-parse stx
    [(_ name (field ...) stuff ...)
     (with-syntax ([with-board (format-id stx "with-~a" (syntax-e #'name))])
       #'(begin
           (struct name (field ...) stuff ...)
           (define-syntax (with-board stx)
             (syntax-parse stx
               [(_ n:expr e (... ...))
                (with-syntax ([field (datum->syntax stx 'field)] ...)
                  #'(let ()
                      (match-define (name field ...) n)
                      e (... ...)))]))))]))