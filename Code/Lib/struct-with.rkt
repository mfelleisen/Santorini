#lang racket

;; ---------------------------------------------------------------------------------------------------
(provide
 ;; SYNTAX
 ;; (with struct:id s-instance:expr e ...)
 ;; struct refers a structure type, s-instance evaluates to an instance of struct
 ;; binds the field names of struct in e ... to the fields of s-instance 
 with)

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax racket/struct-info))
(require (for-syntax syntax/parse))
(require (for-syntax racket/list))

(module+ test (require rackunit))
(module+ test (require syntax/macro-testing))

;; ---------------------------------------------------------------------------------------------------
(define-syntax (with stx)
  (syntax-parse stx
    [(_ structure-type:id instance:expr e ...)
     (define-values (predicate accessors fields) (structure-type->predicate+accessors+fields stx))
     (with-syntax ([predicate?     predicate]
                   [(accessor ...) accessors]
                   [(field ...)    fields])
       #`(let* ([v instance]
                [_ (unless (predicate? v)
                     (error 'with "expected struct instance of type ~a, given ~e" 'structure-type v))]
                [field (accessor v)] ...)
           e ...))]))

(define-for-syntax (structure-type->predicate+accessors+fields stx)
  (define s:type (syntax-parse stx [(_ structure-type . x) #'structure-type]))
  (define s:info (extract-struct-info (syntax-local-value s:type (not-a-structure-type s:type))))
  (define-values (_ constructor predicate accessors _2 _3) (apply values s:info))
  (define accessor-field-pattern (string-append (symbol->string (syntax-e s:type)) "-(.*)"))
  (define fields
    (for/list ((a:stx accessors))
      (define a:sym (symbol->string (syntax-e a:stx)))
      (define f:str (regexp-match accessor-field-pattern a:sym))
      (define field (datum->syntax stx (string->symbol (second f:str))))
      field))
  (values predicate accessors fields))

(define-for-syntax ((not-a-structure-type stype))
  (raise-syntax-error #f "not a structure type" stype))

(module+ test
  (struct foo (bar moo))
  (check-equal? (with foo (foo 10 20) (+ moo bar)) 30)
  (check-exn exn:fail:syntax? (lambda () (convert-compile-time-error (with bar (foo 20 0) 0)))))