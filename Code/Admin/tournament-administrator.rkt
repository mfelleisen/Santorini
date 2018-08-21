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
;; A player that fails or cheats gets eliminated and all of its past results are counted in favor of
;; its opponents.

(provide
 tournament-manager)

;; ---------------------------------------------------------------------------------------------------
(require "referee.rkt")
(require "referee-interface.rkt")

(module+ test
  (require rackunit)
  (require "../Player/player.rkt")
  (require "../Player/failing-player.rkt")
  (require "../Lib/xsend.rkt"))

;; ---------------------------------------------------------------------------------------------------

;; type Result = [List String[winner] String[loser]]

;; [Listof [List String Player]] -> [Listof Result]
;; determine the winners of a round-robin tourhament 
(define (tournament-manager lop)
  (define schedule (all-pairings lop))
  ;; [Listof [List String[winner] String[loser]]] __
  (define-values (results _cheaters)
    (for/fold ([results '()][cheaters '()])
              ((pairing schedule)
               #:unless (or (member (first (first pairing)) cheaters)
                            (member (first (second pairing)) cheaters)))
      (match-define (list (list name1 player1) (list name2 player2)) pairing)
      (define referee (new referee% [one player1][two player2]))
      (define result  (send referee best-of 3))
      (match result
        [(? string? winner)
         (define loser (other-one winner name1 name2))
         (values (cons (list winner loser) results) cheaters)]
        [(terminated winner reason)
         (define loser (other-one winner name1 name2))
         (values (cons (list winner loser) (purge loser results cheaters)) (cons loser cheaters))])))
  results)

;; String String String -> String 
(define (other-one winner name1 name2)
  (if (string=? winner name1) name2 name1))

;; String [Listof Result] [Listof String] -> [Listof Result]
;; flip the results of any past games 
(define (purge name results cheaters)
  (for/fold ((purged '())) ((r results))
    (match-define `(,winner ,loser) r)
    (if (string=? winner name)
        (if (member loser cheaters)
            purged
            (cons (list loser winner) purged))
        (cons r purged))))

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
  (time-out-limit 1.2)

  (check-equal? (all-pairings '(a b)) '((a b)))

  (define-syntax-rule
    (check-tm players expected msg)
    (check-equal? (let ([op (open-output-string)])
                    (begin0
                      (parameterize ((current-output-port op))
                        (tournament-manager players))))
                  expected msg))
  
  (define (make-player name player%)
    (list name (new player% [name name])))

  (define tournament (list (make-player "matthias" player%) (make-player "christos" player%)))
  (define (baddy%) (make-failing-player% 1 #:p-failure (lambda (l) (if (empty? l) '(0 -1) (caar l)))))
  (define (tournament+bad-pl) (cons (make-player "baddy" (baddy%)) tournament))
  (define (baddy-tt%) (make-failing-player% 2 #:tt-failure (lambda (board) (/ 1 0))))
  (define (tournament+bad-pl+bad-tt) (cons (make-player "baddytt" (baddy-tt%)) (tournament+bad-pl)))

  (check-tm tournament           '(("matthias" "christos")) "plain 1")
  (check-tm (reverse tournament) '(("christos" "matthias")) "plain 2")
  (check-tm (tournament+bad-pl)  '(("matthias" "christos") ("matthias" "baddy")) "bad pl")
  (check-tm (tournament+bad-pl+bad-tt) '(("matthias" "christos") ("matthias" "baddytt")) "bad tt"))
  
