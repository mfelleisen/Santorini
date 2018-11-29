#lang racket

;; =============================================================================
;; a library that protects calls from exceptions and overly slow clients 

(provide

 time-out-limit

 (contract-out

  #;
  (time-out-limit
   ;; set the time-out value (default: 0.1s)
   (-> (and/c real? positive?) any))
  
  (xdynamic-send
   (->i ([o object?] [m symbol?] #:thrown [x (-> any/c any)] #:timed-out [y (-> any)])
        #:rest (m-s-arguments any/c)
        (_ any/c))))

 ;; SYNTAX like xdynamic-send, but no quote 
 xsend)

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))
(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define EXN:fmt "xdynamic-send: ~a raised an exception for ~a:\n~e")

(define time-out-limit (make-parameter .1))

(define-syntax (xsend stx)
  (syntax-parse stx 
    [(xsend o m #:thrown h-thrown #:timed-out h-time-out a ...)
     #'(xdynamic-send o 'm #:thrown h-thrown #:timed-out h-time-out a ...)]))

(define (xdynamic-send target m #:thrown throw-handler #:timed-out time-out-handler  . a)
  (define cust (make-custodian))
  ;; (custodian-limit-memory cust 1048576) ;; memory limit
  (struct okay (value))
  (struct thrw (value))
  (define result
    (parameterize ((current-custodian cust))
      (define results-of-thread (make-channel))
      (thread
       (lambda ()
         (with-handlers ((void (lambda (x) (channel-put results-of-thread (thrw x)))))
           (define result-of-call (apply dynamic-send target m a))
           (channel-put results-of-thread (okay result-of-call)))))
      
      (sync/timeout (time-out-limit) results-of-thread)))
  (custodian-shutdown-all cust)
  (cond
    [(okay? result) (okay-value result)]
    [(false? result)
     (log-error "timed out")
     (time-out-handler)]
    [(thrw? result)
     (define thrown (thrw-value result))
     (log-error (format EXN:fmt target m (if (exn? thrown) (exn-message thrown) thrown)))
     (throw-handler thrown)]
    [else (error 'xdynamic-send "something went horribly wrong: ~e" result)]))

;; ===================================================================================================
(module+ test
  (require (submod ".."))

  (define test%
    (class object%
      (super-new)
      (define/public (good x) GOOD)
      (define/public (better x y) #false)
      (define/public (diverge x y z) (let loop () (loop)))
      (define/public (raise-exn w) (raise 0))))

  (define test (new test%))
  
  (define GOOD 5)
  (struct ex [value])
  (struct tt [])

  (time-out-limit .001)
  (check-equal? (xdynamic-send test 'good #:thrown ex #:timed-out tt 0) GOOD)
  (check-equal? (xdynamic-send test 'better #:thrown ex #:timed-out tt 0 1) #f)
  (check-pred   tt? (xdynamic-send test 'diverge #:thrown ex #:timed-out tt 0 1 2))
  (check-pred   ex? (xdynamic-send test 'raise-exn #:thrown ex #:timed-out tt 3))

  (check-equal? (xsend test good #:thrown ex #:timed-out tt 0) GOOD)
  (check-equal? (xsend test better #:thrown ex #:timed-out tt 0 1) #f)
  (check-pred   tt? (xsend test diverge #:thrown ex #:timed-out tt 0 1 2))
  (check-pred   ex? (xsend test raise-exn #:thrown ex #:timed-out tt 3)))
