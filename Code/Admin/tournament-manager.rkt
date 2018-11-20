#lang racket

;; A tournament pitches n players against each other in a round-robin ("everyone against everyone"].
;; A player is specified with two pieces of information per line
;; -- an identifying name; and
;; -- a path to a file that implements the player mechanics.

;; Each confrontation between two players is run as a "best of 3" game. 
;; A player that fails or cheats gets eliminated and all of its past results are counted in favor of
;; its opponents.

(require "../Common/player-interface.rkt")
(require "../Common/observer-interface.rkt")

(define player-spec/c   (list/c string? string? path-string?))
(define observer-spec/c (list/c string? path-string?))

(provide
 
 (contract-out
  (rename
   tournament-manager main
   ;; configure tournament manager from the information in the specified file 
   (-> (list/c (listof string?) result*/c)))
  
  (tournament-manager/proc
   ;; determine the winners of a round-robin tourhament 
   (-> (listof player/c) (listof observer/c) (list/c (listof string?) result*/c)))

  (create-players
   ;; create players from configuration specs 
   (-> (listof player-spec/c) (listof any/c #;player/c)))

  (create-observers
   ;; create observers from configuration specs 
   (-> (listof observer-spec/c) (listof observer/c)))))

;; ---------------------------------------------------------------------------------------------------
(require "referee.rkt")
; (require (submod "../Common/results.rkt" json))
(require "../Lib/xsend.rkt")
(require json)

(module+ test
  (require rackunit)
  (require "../Player/player.rkt")
  (require "../Player/failing.rkt")
  (require "../Lib/with-output-to-dev-null.rkt"))

;; ---------------------------------------------------------------------------------------------------
(define (tournament-manager)
  (define-values (lop-spec loo-spec) (read-player-info))
  (define lop (create-players lop-spec))
  (define loo (create-observers loo-spec))
  (tournament-manager/proc lop loo))

#; [type PlayerSpec   = [List Kind String PathString]]
#; [type ObserverSpec = [List String PathString]]

#; [-> (values [Listof PlayerSpec] [Listof ObserverSpec])]
;; read player info from STDIN
(define (read-player-info)
  (define configuration (read-json))
  (match configuration
    [(hash-table (observers (and o `((,o-name ,o-path) ...)))
                 (players (and p `((,kind ,p-name ,p-path) ...))))
     (values p o)]
    [else (error 'tournament-manager "hash expected, given ~e" configuration)]))

(define ((make-create class% name-selector mechanics-selector) lop-spec)
  (for/list ((pi lop-spec))
    (define name (name-selector pi))
    (define mechanics (mechanics-selector pi))
      (new (loader mechanics class%) [name name])))

#; [ [Listof PlayerSpec] -> [Listof Player] ]
;; dynamically load player from specified path
(define create-players (make-create 'player% second third))

#; [ [Listof ObserverSpec] -> [Listof Observer] ]
(define create-observers (make-create 'observer% first second))

;; PathString -> Any
;; raise an exception if neither direct dynamic-require nor collection-oriented works 
(define (loader mechanics id)
  (with-handlers ([exn:fail?
                   (lambda (xn)
                     (dynamic-require (string->symbol mechanics) id))])
    (dynamic-require mechanics id)))
  
;; ---------------------------------------------------------------------------------------------------
#; [type Schedule = [Listof [List [List String Player] [List String Player]]]]
#; [type Result   = [Listof [List String[winner] String[loser]]]]

(define (tournament-manager/proc lop0 loo)
  (define lop       (assign-unique-names lop0))
  (define schedule  (all-pairings lop))
  (define-values (results cheaters) (run-tournament schedule loo))
  (define cheaters+ (inform-all-non-cheaters lop cheaters results))
  (list cheaters+ results))

#; (Schedule [Listof Observer] -> (values Result [Listtof String]))
(define (run-tournament schedule loo)
  (define-values (results cheaters)
    (for*/fold ([results '()] [cheaters '()])
               ((pairing schedule)
                (name1   (in-value (first (first pairing))))
                [name2   (in-value (first (second pairing)))]
                #:unless (or (member name1 cheaters) (member name2 cheaters)))
      (define player1  (second (first pairing)))
      (define player2  (second (second pairing)))
      (define referee  (new referee% [one player1][one-name name1][two player2][two-name name2]))
      (define ref-with (attach-observers referee loo))
      (define result   (send ref-with best-of 3))
      (match result
        [(? string? winner)
         (define loser (other-one winner name1 name2))
         (values (cons (list winner loser) results) cheaters)]
        [(terminated winner reason)
         (define loser (other-one winner name1 name2))
         (values (cons (list winner loser) (purge loser results cheaters)) (cons loser cheaters))])))
  (values (reverse results) (reverse cheaters)))

#; [ Referee [Listof Observer] -> Referee ]
(define (attach-observers referee loo)
  (for ((o loo)) (send referee register))
  referee)

;; [Listof Player] -> [Listof [List String Player]]
;; EFFECT send those player a "playing as" message whose name coincides with another player 
(define (assign-unique-names players0)
  (let assign-unique-names ([players players0] [names '()] [longest ""])
    (cond
      [(empty? players) '()]
      [else (define player   (first players))
            (define as       (pick-unique-name player names longest))
            (cond
              ;; not a cheater, just a failure
              [(boolean? as) (assign-unique-names (rest players) names longest)] 
              [else (define names+   (cons as names))
                    (define longest+ (argmax string-length (list longest as)))
                    (cons (list as player) (assign-unique-names (rest players) names+ longest+))])])))

;; Player [Listof String] String -> (U #false String)
;; EFFECT send player a playing-as message if name must be changed 
(define (pick-unique-name fst names longest)
  (define nm (get-field name fst))
  (cond
    [(member nm names)
     (define as (make-longer-name nm longest))
     (match (xsend fst playing-as #:thrown vector #:timed-out vector as)
       [(vector)     (displayln `(manager ,nm : "playing-as" method timed out))
                     #false]
       [(vector msg) (displayln `(manager ,nm : "playing-as" method failed ,msg))
                     #false]
       [r            as])]
    [else nm]))

;; [Listof [List String Player]] -> [Listof [List String String Referees]]
(define (all-pairings lop)
  (let loop ([lop lop][others lop])
    (cond
      [(empty? lop) '()]
      [else (define player1  (first lop))
            (define nuothers (remove player1 others))
            (append (for/list ((opponent nuothers)) (list player1 opponent))
                    (loop (rest lop) nuothers))])))

;; String String -> String
;; add a-s to the end of name so that it is one longer than longest
(define (make-longer-name name longest)
  (define n (string-length longest))
  (define l (string-length name))
  (string-append name (make-string (- n l -1) #\a)))

;; String String String -> String 
(define (other-one winner name1 name2)
  (if (string=? winner name1) name2 name1))

;; String [Listof Result] [Listof String] -> [Listof Result]
;; flip the results of any past games 
(define (purge name results cheaters)
  (define purged-results
    (for/fold ((purged '())) ((r results))
      (match-define `(,winner ,loser) r)
      (if (string=? winner name)
          (if (member loser cheaters)
              purged
              (cons (list loser winner) purged))
          (cons r purged))))
  (reverse purged-results))

;; [Listof Player] [Listof String] Results -> [Listof String]
;; EFFECT inform all players on lop, except for those whose name is on the cheater's list 
(define (inform-all-non-cheaters lop cheaters0 results)
  (for*/fold ([cheaters cheaters0])
             ((p lop) (name (in-value (first p))) #:unless (member name cheaters))
    (if (vector? (xsend (second p) end-of-game #:thrown vector #:timed-out vector results))
        (cons name cheaters)
        cheaters)))
        
;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  
  (time-out-limit 9.6)

  (check-equal? (all-pairings '(a b)) '((a b)))
  
  (check-equal? (make-longer-name "matthias" "matthias") "matthiasa")

  (define (check-assign-unique-names-result-and-effect)
    (define pl1 (new player% [name "matthias"]))
    (define pl2 (new player% [name "matthias"]))
    (check-equal? (let* ([result (assign-unique-names (list pl1 pl2))]
                         [effect (get-field name pl2)])
                    (list result effect))
                  (list (list (list "matthias" pl1) (list "matthiasa" pl2)) "matthiasa")))
  (check-assign-unique-names-result-and-effect)

  (check-equal? (purge "b" '(("b" "a") ("c" "b") ("c" "d")) '("a" "b")) '(("c" "b") ("c" "d")))

  ;; -------------------------------------------------------------------------------------------------
  (define-syntax-rule
    (check-tm players expected msg)
    (check-equal? (with-output-to-dev-null (lambda () (tournament-manager/proc players '())))
                  expected
                  msg))
  
  (define failing-after-1-for-placement%
    (make-failing-player% 1 #:p-failure (lambda (l) (if (empty? l) '(0 -1) (caar l)))))
  (define failing-after-3-for-take-turn%
    (make-failing-player% 2 #:tt-failure (lambda (board) (/ 1 0))))

  (define matthias (new player% [name "matthias"]))
  (define christos (new player% [name "christos"]))
  (define baddypl  (new failing-after-1-for-placement% [name "baddypl"]))
  (define baddytt  (new failing-after-3-for-take-turn% [name "baddytt"]))
  (define baddypla (new failing-after-1-for-placement% [name "baddypl"]))

  (define plain          (list matthias christos))
  (define plain+fail-1   (cons baddypl plain))
  (define plain+fail-1+3 (list* baddypl baddytt baddypla plain))
  
  (check-tm plain+fail-1
            '(("baddypl") (("matthias" "baddypl") ("christos" "matthias")))
            "bad pl")

  (check-tm plain+fail-1+3
            '(("baddypl" "baddypla" "baddytt") (("matthias" "baddytt") ("christos" "matthias")))
            "bad tt")

  ;; -------------------------------------------------------------------------------------------------
  (define player-info:json
    #<< eos
{ "players" : [["good", "matthias", "../Player/player.rkt"],
               ["good", "christos", "../Player/player.rkt"]],
  "observers" : []}
 eos
    )

  (define player-info:jsexpr
    '[["good" "matthias" "../Player/player.rkt"]
      ["good" "christos" "../Player/player.rkt"]])

  (let-values ([(p* o*) (with-input-from-string player-info:json read-player-info)])
    (check-equal? p* player-info:jsexpr)
    (check-equal? o* '()))

  (define players (create-players player-info:jsexpr))
    
  (check-tm players '(() (("christos" "matthias"))) "created players")

  (define mixed-player-info:json
    #<< eos
{ "players" : [["good",     "matthias", "../Player/player.rkt"],
               ["good",     "christos", "../Player/player.rkt"],
               ["infinite", "infplace", "../Player/failing-inf-placement.rkt"],
               ["breaker",  "badplace", "../Player/failing-placement.rkt"],
               ["breaker",  "badturn",  "../Player/failing-placement.rkt"],
               ["infinite", "infturn",  "../Player/failing-inf-turn.rkt"]],
  "observers" : []}
 eos
    )

  (let-values ([(p* o*) (with-input-from-string mixed-player-info:json read-player-info)])
    (check-equal? (length p*) 6)
    (check-equal? o* '())
    (define players (create-players p*))
    (define results
      '(("christos" "matthias")
        ("matthias" "infplace")
        ("matthias" "badplace")
        ("matthias" "badturn")
        ("matthias" "infturn")))
    (define outcome (with-output-to-dev-null (lambda () (tournament-manager/proc players '()))))
    (match-define `(,cheaters ,games) outcome)
    (check-equal? cheaters '("infplace" "badplace" "badturn" "infturn") "cheaters for config")
    (check-equal? games    results                                      "games for config")))