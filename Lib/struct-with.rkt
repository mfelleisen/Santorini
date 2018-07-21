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
(module+ test (require rackunit))
(module+ test (require syntax/macro-testing))

;; -----------------------------------------------------------------------------
(define-syntax (struct-with stx)
  (syntax-parse stx
    [(_ name (field ...) stuff ...)
     (with-syntax ([with-board (format-id stx "with-~a" (syntax-e #'name))]
                   [name?      (format-id stx "~a?" (syntax-e #'name))])
       #'(begin
           (struct name (field ...) stuff ...)
           (define-syntax (with-board stx)
             (syntax-parse stx
               [(_ n:expr e (... ...))
                (with-syntax ([field (datum->syntax stx 'field)] ...)
                  #'(let ([v n])
                      (unless (name? v)
                        (error 'with-board "not a ~a: ~e" 'name v))
                      (match-define (name field ...) v)
                      e (... ...)))]))))]))

(module+ test
  (struct-with foo ())
  (check-equal? (with-foo (foo) 0) 0)
  (check-exn exn:fail? (lambda () (convert-compile-time-error  (with-foo 0 (foo))))))