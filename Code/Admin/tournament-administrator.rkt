#! /bin/sh
#| -*- racket -*-
exec racket -tm "$0" ${1+"$@"}
|#

#lang racket

;; A tournament pitches n players against each other in a round-robin ("everyone against everyone"].
;; A player is specified with two pieces of information per line
;; -- an identifying name; and
;; -- a path to a file that implements the player mechanics.

;; Each confrontation between two players is run as a "best of 3" game. 
;; A player that fails or cheats gets eliminated and all of its past results are counter in favor of
;; its opponents.

(provide
 tournament-manager)

;; ---------------------------------------------------------------------------------------------------
(require "referee.rkt")

(module+ test
  (require rackunit))

(module+ main
  (require "../Player/player.rkt")
  (require "../Lib/xsend.rkt"))

;; ---------------------------------------------------------------------------------------------------

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

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (all-pairings '(a b)) '((a b))))

(module+ main
  (define tournament-players
    (list
     (list "matthias" (new player% [name "matthias"]))
     (list "christos" (new player% [name "christos"]))))
  
  (time-out-limit 1.2)
  (tournament-manager tournament-players))
