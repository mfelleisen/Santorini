#lang racket

;; run a tournament from command line

;; ---------------------------------------------------------------------------------------------------
(require "tournament-manager.rkt")
(module+ test
  (require "../Lib/xsend.rkt")
  (require "../Lib/with-output-to-dev-null.rkt")
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (main)
  (tournament-manager (info->players (read-player-info))))

(struct player-info (name mechanics) #:transparent)

;; [Listof PlayerInfo] -> [Listof [List String Player]]
(define (info->players lopi)
  (for/list ((pi lopi))
    (match-define (player-info name mechanics) pi)
    (define player% (dynamic-require mechanics 'player%))
    (new player% [name name])))

;; -> [Listof PlayerInfo]
(define (read-player-info)
  (for/list ((player-information (in-port read-line)))
    (with-input-from-string player-information
      (lambda ()
        (player-info (read-element) (read-element) )))))

;; -> String
(define (read-element)
  (symbol->string (read)))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define player-info:string
    #<< eos
matthias ../Player/player.rkt
christos ../Player/player.rkt
 eos
    )

  (define player-info:struct
    (list
     (player-info "matthias" "../Player/player.rkt")
     (player-info "christos" "../Player/player.rkt")))

  (check-equal? (with-input-from-string player-info:string read-player-info) player-info:struct)
  (check-pred cons? (info->players player-info:struct))

  (time-out-limit 1.2)

  (check-equal? (with-output-to-dev-null ; #:error-port (open-output-string)
                  (lambda () (with-input-from-string player-info:string main)))
                '(() (("matthias" "christos")))))
