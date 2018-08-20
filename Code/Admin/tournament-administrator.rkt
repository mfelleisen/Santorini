#! /bin/sh
#| -*- racket -*-
exec racket -tm "$0" ${1+"$@"}
|#

#lang racket

;; A tournament pitches n players against each other in a round-robin ("everyone against everyone"].
;; A player is specified with three pieces of information per line
;; -- an identifying name;
;; -- a path to a file that implements the player mechanics; and
;; -- a path fo a file that implements a strategy.
;; The input is read from the command line. 

;; Each confrontation between two players is run as a "best of 3" game. 
;; A player that fails or cheats gets eliminated and all of its past results are counter in favor of
;; its opponents.

;; ---------------------------------------------------------------------------------------------------
(require "referee.rkt")
(require "../Player/player.rkt")
(require "../Player/strategy.rkt")

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------

(struct player-info (name mechanics strategy) #:transparent)

(define (command-line-main)
  (tournament-manager (info->players (read-player-info))))

;; [Listof [List String Player]] -> [Listof String]
;; determine the winners of a round-robin tourhament 
(define (tournament-manager lop)
  (define schedule (all-pairings lop))
  (for/fold ([results '()][cheaters '()]) ((pairing schedule))
    (match-define (list (list name1 player1) (list name2 player2)) pairing)
    (define referee (new referee% [one player1][two player2]))
    (define result  (send referee best-of 3))
    (values (cons result results) cheaters)))
      
;; [Listof [List String Player]] -> [Listof [List String String Referees]]
(define (all-pairings lop)
  (let loop ([lop lop][others lop])
    (cond
      [(empty? lop) '()]
      [else (define player1   (first lop))
            (define nuothers  (remove player1 others))
            (append (for/list ((opponent nuothers)) (list player1 opponent))
                    (loop (rest lop) nuothers))])))

;; [Listof PlayerInfo] -> [Listof [List String Player]]
(define (info->players lopi)
  (for/list ((pi lopi))
    (match-define (player-info name mechanics strategy) pi)
    (define player% (dynamic-require mechanics 'player%))
    (list name (new player% [name name]))))

;; -> [Listof PlayerInfo]
(define (read-player-info)
  (for/list ((player-information (in-port read-line)))
    (with-input-from-string player-information
      (lambda ()
        (player-info (read-element) (read-element) (read-element) )))))

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
     (player-info "matthias" "../Player/player.rkt" "../Player/strategy.rkt")
     (player-info "christos" "../Player/player.rkt" "../Player/strategy.rkt")))

  (check-equal? (with-input-from-string player-info:string read-player-info) player-info:struct)
  (check-pred cons? (info->players player-info:struct))

  (check-equal? (all-pairings '(a b)) '((a b))))

(module+ test 
  (tournament-manager (info->players player-info:struct)))
