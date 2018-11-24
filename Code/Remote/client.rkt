#lang racket

;; The client script is responsible for connecting to the server,
;; creating a player, and connecting it to the server via the
;; Remote/tournament-manager. The rest of the interaction is up to
;; the latter.

;; -----------------------------------------------------------------------------
(provide
 client)

;; -----------------------------------------------------------------------------
(require "tournament-manager.rkt")
(require "../Admin/tournament-manager.rkt")
(require "../Lib/io.rkt") 
(require json)

(module+ test
  (require (submod "tournament-manager.rkt" test-support)))

;; -----------------------------------------------------------------------------
(define IP   (make-parameter LOCALHOST))
(define PORT (make-parameter 45678))

;; -> [Listof Results]
(define (client)
  (define ch (make-channel))
  (match-define `(,p* ,o* ,ip ,port) (read-client-configuration))
  (IP   ip)
  (PORT port)
  (define player-thread-evts
    (for/list ((p (create-players p*)))
      (sleep 3)
      (thread-dead-evt
       (thread
        (lambda ()
          (with-handlers ([exn:fail:network? (lambda (xn) (printf "connection failed\n"))])
            (define m (call-with-values (lambda () (tcp-connect (IP) (PORT))) (run-client p)))
            (channel-put ch m)))))))
  (define ch?
    (let loop ([t* player-thread-evts])
      (cond
        [(empty? t*) (channel-get ch) #;(error 'client "no thread sent a result")]
        [else 
         (define s (map (lambda (pte) (handle-evt pte (lambda (t) (loop (remove t t*))))) t*))
         (apply sync ch s)])))
  ch?)

(define (tee tag x)
  (displayln `(,tag ,x) (current-error-port))
  x)

#; [-> (list N Port# Positive 0or1)]
;; read player info from STDIN
(define (read-client-configuration)
  (define configuration (read-json))
  (match configuration
    [(hash-table ('observers (and o* `((,o-name ,o-path) ...)))
                 ('players   (and p* `((,kind ,p-name ,p-path) ...)))
                 ('ip        (and ip-address string?))
                 ('port      (and port-number port#)))
     (list p* o* ip-address port-number)]
    [else (error 'client "hash expected, given ~e" configuration)]))

;; String String -> [InputPort OutputPort -> [Listof Results]]
(define ((run-client player) in out)
  (define manager (tournament-manager in out))
  (manager player))

#; (Number -> (U #false Number))
(define (port# n)
  (and (integer? n) (<= 50000 n 60000) n))

;; -----------------------------------------------------------------------------
(module+ test
  (define DEFAULT-PLAYER "../Player/player.rkt")

  (define (run in out)
    (define player% (dynamic-require DEFAULT-PLAYER 'player%))
    (define player  (new player% [name  "matthias"]))
    (define manager (run-client player))
    (lambda (_player) (manager in out)))
  
  (test-suite run))