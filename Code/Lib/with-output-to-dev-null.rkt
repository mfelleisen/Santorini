#lang racket

(provide
  with-output-to-dev-null)

(define (with-output-to-dev-null cmd #:hide (hide-string #true))
  (let* ((outp (open-output-bytes)))
    (parameterize ((current-output-port outp))
      (cond
	[hide-string (cmd)]
	[else
	  (define result (cmd))
	  (define output (get-output-bytes outp))
	  (values result output)]))))

(module+ test
  (require rackunit)
  (check-equal? (with-output-to-dev-null (lambda () (displayln "hello world") 0)) 0)
  (check-equal? (let-values ([(r o)
			      (with-output-to-dev-null
				(lambda () (display "hello world") 0)
				#:hide #false)])
		  (list r o))
               (list 0 #"hello world")))
    
