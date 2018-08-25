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

(module+ test
  (require (submod "tournament-manager.rkt" test-support)))

;; -----------------------------------------------------------------------------
(define DEFAULT-PLAYER "../Player/player.rkt")

;; String String String [String] -> [Listof Results]
(define (client server port name (player%-file DEFAULT-PLAYER))
  (call-with-values (lambda () (tcp-connect server port)) (run-client name player%-file)))

;; String String -> [InputPort OutputPort -> [Listof Results]]
(define ((run-client name player%-file) in out)
  (define player% (dynamic-require player%-file 'player%))
  (define player  (new player% [name name]))
  (define manager (tournament-manager in out))
  (manager player))

;; -----------------------------------------------------------------------------
(module+ test
  (define (run in out)
    (define manager (run-client "matthias" DEFAULT-PLAYER))
    (lambda (_player) (manager in out)))
  
  (test-suite run))