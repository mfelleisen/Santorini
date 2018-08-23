#lang racket

(define-syntax-rule (boolean-parameter/c . x) (-> boolean? any))
  
(provide
 (contract-out
  (pretty-print-output? (boolean-parameter/c default: #f))
  (trickle-output?      (boolean-parameter/c default: #f))
  (trailing-newline?    (boolean-parameter/c default: #t))
  (encode-all-unicode?  (boolean-parameter/c default: #f))
   
  (send-message (->* (jsexpr?) (output-port?) any))
  (read-message (->* () (input-port?) jsexpr?))

  (LOCALHOST      string?)
  (REMOTE-PORT    (between/c 0 64000))
  (TIMEOUT        (between/c 0 100))
  (ACCEPT-TIMEOUT (between/c 0 100))
  (unset-time-out (-> any))))
  
;; ---------------------------------------------------------------------------------------------------
(require "json-pretty.rkt")
(require json) 

;; ---------------------------------------------------------------------------------------------------

(define LOCALHOST      "127.0.0.1")
(define REMOTE-PORT    45678)
(define ACCEPT-TIMEOUT 5) ;; seconds. See (server).
(define TIMEOUT        5) ;; seconds. See read-json-safely/timeout.

(define (unset-time-out) (set! TIMEOUT 1000000000))

(define pretty-print-output? (make-parameter #f))
(define trickle-output?      (make-parameter #f))
(define trailing-newline?    (make-parameter #t))
(define encode-all-unicode?  (make-parameter #f))

(define (send-message i (op (current-output-port)))
  (with-handlers ([exn:fail:network? (lambda (e) #f)])
    (define output-bytes (format-output i))
    (parameterize ((current-output-port op))
      (if (trickle-output?) (trickle-send-message output-bytes) (write-bytes output-bytes))
      (if (trailing-newline?) (newline) (write-byte 32))
      (flush-output))))

;; Bytes -> Void
(define (trickle-send-message output-bytes)
  (define output-length (bytes-length output-bytes))
  (define chunk-size (max (quotient output-length 100) 10))
  (for [(offset (in-range 0 output-length chunk-size))]
    (define chunk (subbytes output-bytes offset (min output-length (+ offset chunk-size))))
    (write-bytes chunk)
    (flush-output)
    (sleep 0.005)))
  
;; JSexpr -> Bytes 
(define (format-output i)
  (if (pretty-print-output?)
      (with-output-to-bytes (lambda () (write-json/pretty i #:indent-maps? #t #:indent-lists? #t)))
      (jsexpr->bytes i #:encode (if (encode-all-unicode?) 'all 'control))))


;; Read a blob of JSON, treating any network error as EOF, and only waiting for TIMEOUT seconds.
;; (Because tcp-read gets RST from linux servers from time to time.)
(define (read-message (ip (current-input-port)))
  (with-handlers ([exn:fail:network? (lambda (_exn) eof)])
    (parameterize ((current-input-port ip))
      (read-json/timeout TIMEOUT ACCEPT-TIMEOUT))))

;; Read a blob of JSON with a timeout for the first byte of input to appear
;; and a second timeout by which the entirety of the blob should have appeared.
(define (read-json/timeout start-timeout-sec response-duration-timeout-sec)
  (define control-ch (make-channel))
  (define reply-ch (make-channel))
  (define read-thread
    (thread
     (lambda ()
       (cond
         [(sync/timeout start-timeout-sec (current-input-port))
          (channel-put control-ch 'response-started)
          (with-handlers [(values (lambda (e) (channel-put reply-ch (list 'exn e))))]
            (channel-put reply-ch (list 'ok (read-json))))]
         [else (channel-put control-ch 'response-not-started)]))))
  (match (channel-get control-ch)
    ['response-not-started
     (log-info "Timed out waiting for reading to start.")
     'timeout-1]
    ['response-started
     (match (sync/timeout response-duration-timeout-sec reply-ch)
       [(list 'ok blob)
        blob]
       [(list 'exn (? exn:fail:network?))
        eof]
       [(list 'exn e)
        (local-require racket/exn)
        (log-info "Error reading message:\n~a" (exn->string e))
        'error]
       [#f
        (log-info "Timed out waiting for reading to complete.")
        'timeout-2])]))