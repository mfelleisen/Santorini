#lang racket

(provide
 #;  ([X] (-> X) [#:hide : Boolean #true] [#:error-port : (U Boolean Output-port) #false]
          ->
          (U X [List X String] [List X String String]))
 ;; by default, hide STDOUT and show STDERR; return the result of (cmd)
 ;; if #:hide is #false, return (list (cmd) output-as-bytes)
 ;; if #:error-port is a port, return (list (cmd) output-as-bytes error-output-as-byte
 with-output-to-dev-null)

;; ---------------------------------------------------------------------------------------------------
(define (with-output-to-dev-null cmd #:hide (hide-string #true) #:error-port (ep #false))
  (let* ((outp (open-output-bytes))
         (errp (open-output-bytes)))
    (parameterize ((current-output-port outp)
                   (current-error-port  (if ep errp (current-error-port))))
      (cond
        [hide-string (cmd)]
        [else
         (define result (cmd))
         (define output (get-output-bytes outp)) 
         (define errors (get-output-bytes errp))         
         (if ep
             (append (list result output) (list errors))
             (list result output))]))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit)
  (check-equal? (with-output-to-dev-null (lambda () (displayln "hello world") 0)) 0)
  (check-equal? (with-output-to-dev-null (lambda () (display "hello world") 0) #:hide #false)
                (list 0 #"hello world"))
  (check-equal? (with-output-to-dev-null (lambda () (display "ouch!" (current-error-port)) 0)
                                         #:error-port #true
                                         #:hide #false)
                (list 0 #"" #"ouch!")))
    
