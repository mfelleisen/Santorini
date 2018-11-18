#lang racket

;; The server listens on TCP _port_, accepting connections from clients until enoough
;; of them have connected or at least two players have signed up and a certain _time_
;; has passed. At that point, each connection is turned into a remote player, all of
;; which are handed to a tournament manager, which runs a compettiton of every player
;; against every other player. 

(provide
 ;; -> [Listof Result]
 (rename-out (server main))
 ;; [Port#] [N] -> [Listof Result]
 server)

;; ---------------------------------------------------------------------------------------------------
(require "player.rkt")
(require "../Admin/tournament-manager.rkt")
(require "../Lib/xsend.rkt") (time-out-limit 1.2)
(require "../Lib/io.rkt")    (unset-time-out)
(require json)

(module+ test
  (require "client.rkt")
  (require "../Lib/with-output-to-dev-null.rkt")
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define WAIT-FOR    (make-parameter    10))
(define MIN-PLAYERS (make-parameter     2))
(define PORT        (make-parameter 45678))
(define REPEAT      (make-parameter #false))
(define MAX-TCP     30)
(define REOPEN      #t)

(define (server)
  (match-define `(,m ,p ,w ,r) (read-server-configuration))
  (MIN-PLAYERS m)
  (WAIT-FOR    w)
  (PORT        p)
  (REPEAT      r)
  (server-proper))

;; -> Void
;; set up custodian, start listening on TCP, and collect players
(define (server-proper)
  (define c (make-custodian))
  (parameterize ((current-custodian c))
    (define listener (tcp-listen (PORT) MAX-TCP REOPEN))
    (log-info "listening")
    (collect listener (WAIT-FOR))))

#; [-> (list N Port# Positive 0or1)]
;; read player info from STDIN
(define (read-server-configuration)
  (define configuration (read-json))
  (match configuration
    [(hash-table (|min players| (and m (? natural-number/c)))
                 (port          (and p (? port#)))
                 (|waiting for| (and w postiive-number/c))
                 (repeat        (and r (or 0 1))))
     (list m p w r)]
    [else (error 'server "hash with four fields expected (see spec.), given ~e" configuration)]))

#; (Number -> (U #false Number))
(define (port# n)
  (and (integer? n) (<= 50000 n 60000) n))

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
    (if (< player#+ (MIN-PLAYERS))
        (collect-up-to-min-players players+ player#+)
        (collect-additional-players players+)))
  (define (collect-additional-players players)
    (if (sync/timeout time-limit listener)
        (collect-additional-players (add-player players))
        (sign-up->start-up players)))
  ;; Players -> Players
  ;; EFFECT accept a connection on the listener 
  (define (add-player players)
    (define-values (in out) (tcp-accept listener))
    (define nm (parameterize ((current-input-port in) (current-output-port out)) (read-message)))
    (displayln `(,nm signed up))
    (define pl (new (make-remote-player% in out) [name nm]))
    (cons pl players))
  ;; -- IN -- 
  (collect-up-to-min-players '() 0))

;; [Listof ExternalPlayer] -> Void
;; EFFECT run a complete game of Evolution 
(define (sign-up->start-up players)
  (displayln `(,players playing))
  (begin0
    (tournament-manager/proc players '())
    (custodian-shutdown-all (current-custodian))))

;; ---------------------------------------------------------------------------------------------------
(module+ test


  (define sample-server-config
    #<< eos
  { "min players" : 3,
    "port"        : 56789,
    "waiting for" : 10,
    "repeat"      : 0
  }
 eos
    )

  (check-equal? (with-input-from-string sample-server-config read-server-configuration)
                (list 3 56789 10 0))

  ;; -------------------------------------------------------------------------------------------------
  (define ((run-client ch name))
    (channel-put ch (client LOCALHOST (PORT) name)))

  (define-syntax-rule
    (tester ch [(ch1 name1) (ch2 name2) (ch3 name3) ...] checks ...)
    (let ([ch  (make-channel)]
          [ch1 (make-channel)]
          [ch2 (make-channel)]
          [ch3 (make-channel)] ...)
      (thread
       (lambda ()
         (define result (with-output-to-dev-null server-proper))
         (channel-put ch result)))
      (for ([name (list name1 name2 name3 ...)][ch (list ch1 ch2 ch3 ...)])
        (sleep 1) ;; this way matthias signs up first
        (thread (run-client ch name)))
      ;; now test 
      checks ...))

  (define name1 "matthias")
  (define name2 "christos")
  (define expected (list (list name2 name1)))
  (tester ch ([ch1 name1][ch2 name2])
          ;; --- running ... then test:
          (check-equal? (channel-get ch1) expected)
          (check-equal? (channel-get ch2) expected)
          (check-equal? (channel-get ch) (list '() expected))))
          
