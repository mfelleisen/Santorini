#lang racket

;; The server listens on TCP _port_, accepting connections from clients.
;; Each connection is turned into a proxy player, after it sends over its name (see BUG).
;; When there are enough proxy players, it signs up additional players for the specified min time.
;; At that point, the players are handed to a tournament manager, which runs a round-robin compettiton

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
  (require "../Common/results.rkt")
  (require "../Lib/with-output-to-dev-null.rkt")
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define MAX-TCP     30)
(define REOPEN      #t)

;; EFFECT sets four parameters (min players, wait time, port #, and whether the server is to repeat 
;; tournaments; then it tail-calls server-proper 
(define (server)
  (match-define `(,min-players ,port ,wait-for ,_repeat) (read-server-configuration))
  (server-proper min-players port wait-for))

#; [-> (list N Port# Positive 0or1)]
;; read configuration info from STDIN
(define (read-server-configuration)
  (define configuration (read-json)) ;; This may raise exn; deserves better error message for user
  (match configuration
    [(hash-table (|min players| (and m (? natural-number/c)))
                 (port          (and p (? port#)))
                 (|waiting for| (and w (? number? integer? positive?)))
                 (repeat        (and r (or 0 1))))
     (list m p w r)]
    [else (error 'server "hash with four fields expected (see spec.), given ~e" configuration)]))

#; (type Port# satisfies port#)
#; (Number -> (U #false Number))
(define (port# n)
  (and (integer? n) (<= 50000 n 60000) n))

#; (N Port# Positive-> Results)
;; set up custodian, start listening on TCP, then 
;; 1. accept players until there are >= MIN-PLAYERS
;; 2. wait for at most time-limit seconds for next player to sign up, then run a tournament
(define (server-proper min-players port time-limit)
  (define c (make-custodian))
  (parameterize ((current-custodian c))
    (define listener (tcp-listen port MAX-TCP REOPEN))
    (log-error (~a 'listening))

    (let collect-up-to-min-players ((players '()))
      (define players+ (add-player players listener))
      (cond
        [(< (length players+) min-players) (collect-up-to-min-players players+)]
        [else
         (let collect-additional-players ((players players+))
           (if (sync/timeout time-limit listener)
               (collect-additional-players (add-player players))
               (sign-up->start-up (reverse players))))]))))

#; (type Players = [Listof [List String String RemotePlayer]])
;;                               name playing-as player

#; (Players TCPListener -> Players)
;; EFFECT accept a connection on the listener 
(define (add-player players listener)
  (define-values (in out) (tcp-accept listener))
  ;; BUG: read-message may raise an exception when a player doesn't supply a name 
  (define nm (parameterize ((current-input-port in) (current-output-port out)) (read-message)))
  (log-error (~a `(,nm signed up)))
  (define pl (new (make-remote-player% in out) [name nm]))
  (cons pl players))

#; ([Listof ExternalPlayer] -> Results)
;; EFFECT run a complete game of Evolution 
(define (sign-up->start-up players)
  (log-error (~a `(,players playing)))
  (begin0
    (tournament-manager/proc players '())
    (custodian-shutdown-all (current-custodian))))

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
          (check-equal? (channel-get server-result:ch) expected))

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

  (define expected-1 `[["matthias" "infplace" ,IRREGULAR] ["matthias" "infturn" ,IRREGULAR]])
  (tester ch
          client-config-1
          server-config-1
          (check-true (channel? ch))
          (check-equal? (channel-get ch) expected-1))
  )
          
