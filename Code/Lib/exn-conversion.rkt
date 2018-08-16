#lang racket

(provide runtime-exn->syntax-exn)

(define ((runtime-exn->syntax-exn this-syntax) xn)
  (define msg (exn-message xn))
  (define cnm (exn-continuation-marks xn))
  (define stx (list this-syntax))
  (raise (exn:fail:syntax msg cnm stx)))