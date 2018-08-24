#lang racket

;; A remote tournament manager consumes a single player, plus ports on which to communicate.
;; It receives messages on behalf of this player, turns them into method arguments, calls
;; the appropriate methods, and then sends the responses (if any) 

(require "../Common/player-interface.rkt")

(define result/c (list/c string? #;=winner string? #;=loser))

(provide
 (contract-out
  (tournament-manager
   ;; [Listof [List String Player]] -> [Listof Result]
   ;; determine the winners of a round-robin tourhament 
   (-> input-port? output-port? (-> player/c (listof result/c))))))

;; ---------------------------------------------------------------------------------------------------
(require (submod "../Admin/tournament-manager.rkt" json))
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
          (define r (xsend player method #:thrown vector #:timed-out vector x))
          ;; --- this is where I need to check for 3 results so we can log errors in protocol/contract
          (cond
            [(vector? r) (error 'manager "the server violated the game protocol ~e" (vector-ref r 0))]
            [else (begin (when -> (send-message (-> r))) (loop))])))

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
  
  (define (make-game received-messages (op #false))
    (lambda ()
      (define matthias (new player% [name "matthias"])) 
      (define in (open-input-string received-messages))
      ((tournament-manager in (or op (current-output-port))) matthias)))

  (define-syntax check-manager
    (syntax-rules ()
      [(_ rm0 sm0)
       (aux-manager ([rm rm0][sm sm0][game (make-game rm)][win '(("matthias" "christos"))])
                    (check-equal? (with-output-to-dev-null #:hide #false game) `(,win ,sm)))]
      [(_ pred? rm0 sm0)
       (aux-manager ([rm rm0][sm sm0][op (open-output-bytes)][game (make-game rm op)])
                    (check-exn pred? game)
                    (check-equal? (get-output-bytes op) sm))]))

  (define-syntax-rule
    (aux-manager ([rm ((received-msg ...) ...)] [sm ((sent-msg ...) ...)] [x rhs] ...) checks ...)
    (let* ([rm (string-append (jsexpr->string received-msg ...) ...)]
           [sm (bytes-append (string->bytes/locale (jsexpr->string sent-msg ...)) ...)]
           [x  rhs] ...)
      checks ...))
  
  (trailing-newline? #f)
  
  (check-manager
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
    (action->jsexpr (winning-move (worker "matthias1") EAST PUT))))

  (check-manager 
   #px"closed the connection"
   ;; --- received messages 
   ((values "christos")
    (placements->jsexpr '())
    (placements->jsexpr '(("christos" 0 0) ("matthias" 1 1))))
   ;; --- sent messages 
   ((values "matthias")
    (place->jsexpr '(0 0))
    (place->jsexpr '(5 5))))

  (check-manager 
   #px"unexpected message"
   ;; --- received messages 
   ((values "christos")
    (placements->jsexpr '())
    (values '("matthias" "christos" "jared")))
   ;; --- sent messages 
   ((values "matthias")
    (place->jsexpr '(0 0))))

  (check-manager 
   #px"violated the game protocol"
   ;; --- received messages 
   ((values "christos")
    (placements->jsexpr '())
    (placements->jsexpr '(("christos" 0 0))))
   ;; --- sent messages 
   ((values "matthias")
    (place->jsexpr '(0 0)))))
