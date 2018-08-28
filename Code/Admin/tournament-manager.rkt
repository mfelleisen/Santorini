#lang racket

;; A tournament pitches n players against each other in a round-robin ("everyone against everyone"].
;; A player is specified with two pieces of information per line
;; -- an identifying name; and
;; -- a path to a file that implements the player mechanics.

;; Each confrontation between two players is run as a "best of 3" game. 
;; A player that fails or cheats gets eliminated and all of its past results are counted in favor of
;; its opponents.

(require "../Common/player-interface.rkt")

(provide
 (contract-out
  (tournament-manager
   ;; determine the winners of a round-robin tourhament 
   (-> (listof player/c) result*/c))))

;; ---------------------------------------------------------------------------------------------------
(require "referee.rkt")
(require (submod "../Common/results.rkt" json))

(module+ test
  (require rackunit)
  (require "../Player/player.rkt")
  (require "../Player/failing-player.rkt")
  (require "../Lib/with-output-to-dev-null.rkt")
  (require "../Lib/xsend.rkt"))

;; ---------------------------------------------------------------------------------------------------
(define (tournament-manager lop0)
  ;; [Listof [List String Player]]
  ;; ASSERT the strings form a set 
  (define lop (assign-unique-names lop0))
  
  ;; [Listof [List [List String Player] [List String Player]]]
  (define schedule (all-pairings lop))

  (pretty-print (map (lambda (p) (list (caar p) (caadr p))) schedule)
                (current-error-port))
  
  ;; [Listof [List String[winner] String[loser]]] 
  (define-values (results _cheaters)
    (for/fold ([results '()] [cheaters '()])
              ((pairing schedule)
               #:unless (or (member (first (first pairing)) cheaters)
                            (member (first (second pairing)) cheaters)))
      (displayln `(tournament cheaters ,cheaters) (current-error-port))
      (displayln `(tournament playin ,(caar pairing) vs ,(caadr pairing)) (current-error-port))
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


;; [Listof Player] -> [Listof [List String Player]]
;; EFFECT send those player a "playing as" message whose name coincides with another player 
(define (assign-unique-names players0)
  (let assign-unique-names ([players players0] [names '()] [longest ""])
    (cond
      [(empty? players) '()]
      [else (define player   (first players))
            (define as       (pick-unique-name player names longest))
            (define names+   (cons as names))
            (define longest+ (longer> longest as))
            (cons (list as player) (assign-unique-names (rest players) names+ longest+))])))

;; Player [Listof String] String -> String
;; EFFECT send player a playing-as message if name must be changed 
(define (pick-unique-name fst names longest)
  (define nm (get-field name fst))
  (cond
    [(member nm names)
     (define as (make-longer-name nm longest))
     (send fst playing-as as)
     as]
    [else nm]))

;; String String -> String
;; add a-s to the end of name so that it is one longer than longest
(define (make-longer-name name longest)
  (define n (string-length longest))
  (define l (string-length name))
  (string-append name (make-string (- n l -1) #\a)))

;; String *-> String
(define (longer> . s)
  (argmax string-length s))

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

  (check-equal? (longer> "matthias" "") "matthias")
  (check-equal? (make-longer-name "matthias" "matthias") "matthiasa")

  (define (check-assign-unique-names-result-and-effect)
    (define pl1 (new player% [name "matthias"]))
    (define pl2 (new player% [name "matthias"]))
    (check-equal? (let* ([result (assign-unique-names (list pl1 pl2))]
                         [effect (get-field name pl2)])
                    (list result effect))
                  (list (list (list "matthias" pl1) (list "matthiasa" pl2)) "matthiasa")))
  (check-assign-unique-names-result-and-effect)
  
  (define-syntax-rule
    (check-tm players expected msg)
    (check-equal? (with-output-to-dev-null #:error-port (open-output-string)
                    (lambda () (tournament-manager players)))
                  expected
                  msg))
  
  (define plain (list (new player% [name "matthias"]) (new player% [name "christos"])))
  (define failing-after-1-for-placement%
    (make-failing-player% 1 #:p-failure (lambda (l) (if (empty? l) '(0 -1) (caar l)))))
  (define plain+fail-1 (cons (new failing-after-1-for-placement% [name "baddypl"]) plain))
  (define failing-after-3-for-take-turn%
    (make-failing-player% 2 #:tt-failure (lambda (board) (/ 1 0))))
  (define plain+fail-1+3
    (list* (new failing-after-3-for-take-turn% [name "baddytt"])
           (new failing-after-1-for-placement% [name "baddypl"])
           plain+fail-1))

  (check-tm plain           '(("matthias" "christos")) "plain 1")
  (check-tm (reverse plain) '(("christos" "matthias")) "plain 2")
  (check-tm plain+fail-1    '(("matthias" "christos") ("matthias" "baddypl")) "bad pl")

  (check-tm plain+fail-1+3  '(("matthias" "christos")
                              ("baddypla" "christos")
                              ("baddypla" "matthias")
                              ("baddypla" "baddytt"))
            "bad tt")

#| rationale
   | ------------------------ | ------------------------------- |
   | scheduled pairing        | failure or not?  | winner       |
   | ------------------------ | ------------------------------- |
   | ("baddytt" "baddypl")    | bodypl must fail | bodytt wins  |
   | ("baddytt" "baddypla")   | bodytt must fail | bodypla wins |
   | ("baddypla" "matthias")  | bodypla below 0  | bodypla wins |
   | ("baddypla" "christos")  | bodypla below 0  | bodypla wins |
   | ("matthias" "christos")  | n/a              | matthias wins|
   | ------------------------ | ------------------------------- |#)

