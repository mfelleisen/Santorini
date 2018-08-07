#lang racket

(provide
 ;; SYNTAX
 require+
 require-)

(define-syntax-rule (require+ path x ...) (require (only-in path x ...)))

(define-syntax-rule (require- path x ...) (require (except-in path x ...)))