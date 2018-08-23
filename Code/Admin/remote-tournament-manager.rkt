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
(require (submod "../Common/actions.rkt" json))
(require (submod "../Common/board.rkt" json))
(require (submod "../Common/placements.rkt" json))
(require "../Lib/io.rkt")

(module+ test
  (require rackunit)
  (require "../Player/player.rkt")
  (require "../Player/failing-player.rkt")
  (require "../Lib/with-output-to-dev-null.rkt")
  (require "../Lib/xsend.rkt"))

;; ---------------------------------------------------------------------------------------------------
(define ((tournament-manager in out) player)
  (define name (get-field name player))

  (parameterize ([current-input-port in]
                 [current-output-port out])
    (with-handlers ((exn:fail? (lambda (xn) (log-info (format "remote UA: ~e" (exn-message xn))))))
      ;; register the player with the server-side tournament manager 
      (send-message name out)

      ;; deal with all game interactions from, and back to, the server-side referee 
      (let/ec return 
        (let loop ()
          (define message (read-message in))
          (define response 
            (cond
              [(string? message) (send player other message)]
              [(jsexpr->placements message)
               => (lambda (p) (send-message (place->jsexpr (send player placemement p))))]
              [(jsexpr->board message)
               => (lambda (b) (send-message (action->jsexpr (send player take-turn b))))]
              [else (return message)]))
          (loop))))))