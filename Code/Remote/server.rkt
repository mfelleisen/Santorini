#lang racket

;; The server listens on TCP _port_, accepting connections from clients until enoough
;; of them have connected or at least two players have signed up and a certain _time_
;; has passed. At that point, each connection is turned into a remote player, all of
;; which are handed to a tournament manager, which runs a compettiton of every player
;; against every other player. 

(provide
 ;; [String] [String] -> [Listof Result]
 (rename-out (server main))
 ;; [Port#] [N] -> [Listof Result]
 server)

;; ---------------------------------------------------------------------------------------------------
(require "player.rkt")
(require "../Admin/tournament-manager.rkt")
(require "../Lib/xsend.rkt") (time-out-limit 1.2)
(require "../Lib/io.rkt")    (unset-time-out)

(module+ test
  (require "client.rkt")
  (require "../Lib/with-output-to-dev-null.rkt")
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define WAIT-FOR    10)
(define MIN-PLAYERS 2)
(define PORT        45678)
(define MAX-TCP     30)
(define REOPEN      #t)

(define (server (iport0 PORT) (time0 WAIT-FOR))
  (define port (coerce-to-string iport0 port# "expected port number, given ~e"))
  (define time (coerce-to-string time0 sec# "expected time limit (in sec), given ~e"))
  (main-internal port time))

;; (U String Number) FormatString [Number -> Number] -> Number
;; EFFECT signal an error 
(define (coerce-to-string iport0 good-number fmt)
  (cond
    [(good-number iport0) iport0]
    [(string->number iport0) => good-number]
    [else (error 'server fmt iport0)]))

;; Number -> Number 
(define (port# n)
  (and (integer? n) (<= 0 n 64000) n))

;; Number -> Number 
(define (sec# n)
  (and (integer? n) (> n 0)))

;; Port# -> Void 
(define (main-internal port time-limit)
  (define c (make-custodian))
  (parameterize ((current-custodian c))
    (define listener (tcp-listen port MAX-TCP REOPEN))
    (log-info "listening")
    (collect listener time-limit)))

;; Tcp-listener N -> [Lstof Result]
;; 1. accept players until there are >= MIN-PLAYERS
;; 2. wait for at most time-limit seconds for next player to sign up
;; then run a tournament
(define (collect listener time-limit)
  ;; Players = [Listof [List String String RemotePlayer]
  ;;                         name playing-as player 
  (define (collect-up-to-min-players players player#)
    (define players+ (add-player players))
    (define player#+ (+ player# 1))
    (if (< player#+ MIN-PLAYERS)
        (collect-up-to-min-players players+ player#+)
        (collect-additional-players players+)))
  (define (collect-additional-players players)
    (if (sync/timeout time-limit listener)
        (collect-additional-players (add-player players))
        (sign-up->start-up (map third players))))
  ;; Players -> Players
  ;; EFFECT accept a connection on the listener 
  (define (add-player players)
    (define-values (in out) (tcp-accept listener))
    (parameterize ((current-input-port in) (current-output-port out))
      (define nm (read-message))
      (log-info "~a signed ip" nm)
      (define pl (new (make-remote-player% in out) [name nm]))
      (cons pl players)))
  ;; -- IN -- 
  (collect-up-to-min-players '() 0))

;; [Listof ExternalPlayer] -> Void
;; EFFECT run a complete game of Evolution 
(define (sign-up->start-up players)
  (log-info "playing a game with ~a players" (length players))
  (begin0
    (tournament-manager players)
    (custodian-shutdown-all (current-custodian))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define (tester name1 name2)
    (define ch (make-channel))
    (thread (lambda () (channel-put ch (with-output-to-dev-null server))))
    (sleep 1)
    (thread (lambda () (client LOCALHOST PORT name1)))
    (sleep 1) ;; this way matthias signs up first 
    (thread (lambda () (client LOCALHOST PORT name2)))
    ;; --- running ... then test:
    (check-equal? (channel-get ch) (list (list name1 name2))))
  
  (tester "matthias" "chrirstos"))