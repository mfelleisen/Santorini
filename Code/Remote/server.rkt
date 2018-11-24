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
    (collect listener (WAIT-FOR) c)))

#; [-> (list N Port# Positive 0or1)]
;; read configuration info from STDIN
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

;; Tcp-listener N  Custodian -> [Lstof Result]
;; 1. accept players until there are >= MIN-PLAYERS
;; 2. wait for at most time-limit seconds for next player to sign up
;; then run a tournament
(define (collect listener time-limit c)
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
        (sign-up->start-up (reverse players) c)))
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

;; [Listof ExternalPlayer] Custodian -> Void
;; EFFECT run a complete game of Evolution 
(define (sign-up->start-up players c)
  (displayln `(,players playing))
  (begin0
    (tournament-manager/proc players '())
    (custodian-shutdown-all c)))

;; ---------------------------------------------------------------------------------------------------
(module+ test

    (define sample-client-config
      #<< eos
{ "players"   : [["good", "matthias", "../Player/player.rkt"],
                 ["good", "christos", "../Player/player.rkt"]],
  "observers" : [],
  "ip"        : "127.0.0.1",
  "port"      : 55555
}
 eos
      )

  (define sample-server-config
    #<< eos
{ "min players" : 2,
  "port"        : 55555,
  "waiting for" : 5,
 "repeat"      : 0
}
 eos
    )

  (check-equal? (with-input-from-string sample-server-config read-server-configuration)
                (list 2 55555 5 0))

  ;; -------------------------------------------------------------------------------------------------
  (define-syntax-rule
    (tester ch sample-client-config sample-server-config checks ...)
    (let ([ch  (make-channel)])
      (thread
       (lambda ()
         (define result
           (with-output-to-dev-null
            (lambda ()
              (with-input-from-string sample-server-config server))))
         (channel-put ch result)))
      (with-output-to-dev-null
       (lambda ()
         (with-input-from-string sample-client-config client)))
      ;; now test
      checks ...))

  (define name1 "matthias")
  (define name2 "christos")
  (define expected (list (list name1 name2)))
  (tester server-result:ch
          sample-client-config
          sample-server-config
          (check-true (channel? server-result:ch))
          (check-equal? (channel-get server-result:ch) (list '() expected)))

  (define server-config-1
    #<< eos
   { 
   "min players" : 3,
   "port"        : 55555,
   "waiting for" : 5,
   "repeat"      : 0
  }
 eos
    )

  (define client-config-1
    #<< eos
{ 
  "players"   : [["good",     "matthias", "Santorini/Code/Player/player"],
                 ["infinite", "infplace", "Santorini/Code/Player/failing-inf-placement"],
                 ["infinite", "infturn",  "Santorini/Code/Player/failing-inf-turn"]],
  "observers" : [],
  "ip"        : "127.0.0.1",
  "port"      : 55555
}
 eos
    )

  (define expected-1 '[["infplace" "infturn"] [["matthias" "infplace"] ["matthias" "infturn"]]])
  (tester ch
          client-config-1
          server-config-1
          (check-true (channel? ch))
          (check-equal? (channel-get ch) expected-1))
)
          
