#lang racket

(provide
 #;  ([X] (-> X) [#:hide : Boolean #true] [#:error-port : Boolean #false]
          ->
          (U X [List X String] [List X String String]))
 ;; by default, hide STDOUT and show STDERR; return the result of (cmd)
 ;; if #:hide is #false, return (list (cmd) output-as-bytes)
 ;; if #:error-port is #true, return (list (cmd) output-as-bytes error-output-as-byte
 with-output-to-dev-null)

;; ---------------------------------------------------------------------------------------------------
(define (with-output-to-dev-null cmd #:hide (hide-string #true) #:error-port (ep #false))
  (let* ((outp (open-output-bytes))
         (errp (open-output-bytes)))
    (parameterize ((current-output-port outp)
                   (current-error-port  (if ep errp (current-error-port))))
      (define result (cmd))
      (flush-output outp)
      (flush-output errp)
      (define output (get-output-bytes outp))
      (define errors (get-output-bytes errp))
      (cond
        [(and hide-string       (not ep)) result]
        [(and hide-string       ep)       (list result errors)]
        [(and (not hide-string) ep)       (list result output errors)]
        [(and (not hide-string) (not ep)) (list result output errors)]))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  (require rackunit)

  (check-equal? (with-output-to-dev-null (lambda () (displayln "hello world") 0))
                0
                "hide output and errors")

  (check-equal? (with-output-to-dev-null (lambda () (display "hello world") 0)
                                         #:hide #false)
                (list 0 #"hello world" #"")
                "show output and hide errors")

  (check-equal? (with-output-to-dev-null (lambda () (display "ouch!" (current-error-port)) 0)
                                         #:hide #false
                                         #:error-port #true)
                (list 0 #"" #"ouch!")
                "show output and errors")

  (check-equal? (with-output-to-dev-null (lambda () (write 'hello (current-error-port)))
                                         #:error-port #true)
                (list (void) #"hello")
                "hide output and show errors"))
    
