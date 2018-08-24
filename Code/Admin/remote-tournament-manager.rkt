#lang racket

;; A remote tournament manager consumes a single player, plus ports on which to communicate.
;; It receives messages on behalf of this player, turns them into method arguments, calls
;; the appropriate methods, and then sends the responses (if any) 


;; A player is specified with two pieces of information per line
;; -- an identifying name; and
;; -- a path to a file that implements the player mechanics.

;; Each confrontation between two players is run as a "best of 3" game. 
;; A player that fails or cheats gets eliminated and all of its past results are counted in favor of
;; its opponents.

(require "../Common/player-interface.rkt")

(define result/c (list/c string? #;=winner string? #;=loser))

(provide
 (contract-out
  (tournament-manager
   ;; [Listof [List String Player]] -> [Listof Result]
   ;; determine the winners of a round-robin tourhament 
   (-> input-port? output-port? (-> player/c (listof result/c))))))

;; ---------------------------------------------------------------------------------------------------
(require (submod "tournament-manager.rkt" json))
(require (submod "../Common/actions.rkt" json))
(require (submod "../Common/board.rkt" json))
(require (submod "../Common/placements.rkt" json))
(require "../Lib/io.rkt")
(require "../Lib/xsend.rkt")

(module+ test
  (require rackunit)
  (require "../Player/player.rkt")
  (require "../Player/failing-player.rkt")
  (require "../Lib/with-output-to-dev-null.rkt")
  (require "../Lib/xsend.rkt"))

;; ---------------------------------------------------------------------------------------------------
(define ((tournament-manager in out) player)
  (define name (get-field name player))
  (parameterize ([current-input-port in] [current-output-port out])
    ;; register the player with the server-side tournament manager 
    (send-message name out)

    ;; deal with all game interactions from, and back to, the server-side referee 
    (let loop ()
      
      (define-syntax-rule (ssend method ->)
        (lambda (x)
          (define result-of-call (xsend player method #:thrown values #:timed-out values x))
          ;; --- this is where I need to check for 3 results so we can log errors in protocol/contract
          (when -> (send-message (-> result-of-call)))
          (loop)))

      (define message (read-message in))
      (cond
        [(eof-object? message) (error 'manager "the server unexpectedly closed the connection")]
        [(and (string? message) message) => (ssend other #false)]
        [(jsexpr->placements message)    => (ssend placement place->jsexpr)]
        [(jsexpr->board message)         => (ssend take-turn action->jsexpr)]
        [(jsexpr->results message)          message]
        [else (error 'manager "the server sent an unexpected message: ~e" message)]))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  (require (submod "../Common/board.rkt" test-support))
  (require json)
  
  (define (jsexpr->string ->jsexpr x)
    (with-output-to-string (lambda () (define y (->jsexpr x)) (if (jsexpr? y) (send-message y) y))))
  (define-syntax-rule
    (chk-manager ((received-message0 ...) ...) ((sent-message0 ...) ...))
    (let ()
      (define received-messages (list (jsexpr->string received-message0 ...) ...))
      (define sent-messages (list (string->bytes/locale (jsexpr->string sent-message0 ...)) ...))
      (check-equal? (with-output-to-dev-null
                     #:hide #false
                     (lambda ()
                       (define matthias (new player% [name "matthias"])) 
                       (define in (open-input-string (apply string-append received-messages)))
                       ((tournament-manager in (current-output-port)) matthias)))
                    `((("matthias" "christos")) ,(apply bytes-append sent-messages)))))

  (trailing-newline? #f)
  
  (chk-manager
   ;; --- received messages 
   ((values "christos")
    (placements->jsexpr '())
    (placements->jsexpr '(("christos" 0 0) ("matthias" 1 1)))
    (board->jsexpr (cboard [[0christos1 2matthias1 3] [0christos2 1matthias2 2]]))
    (values '(("matthias" "christos"))))
   ;; --- sent messages 
   ((values "matthias")
    (place->jsexpr '(0 0))
    (place->jsexpr '(5 5))
    (action->jsexpr (winning-move (worker "matthias1") EAST PUT)))))
