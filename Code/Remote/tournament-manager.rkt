#lang racket

;; A remote tournament manager consumes a single player, plus ports on which to communicate.
;; It receives messages on behalf of this player, turns them into method arguments, calls
;; the appropriate methods, and then sends the responses (if any) 

(require "../Common/player-interface.rkt")

(define result/c (list/c string? #;=winner string? #;=loser))

(provide
 (contract-out
  (tournament-manager
   (-> input-port? output-port? (-> player/c result*/c)))))

;; ---------------------------------------------------------------------------------------------------
(require "player.rkt")
(require (submod "../Common/actions.rkt" json))
(require (submod "../Common/board.rkt" json))
(require (submod "../Common/placements.rkt" json))
(require (submod "../Common/results.rkt" json))
(require "../Lib/io.rkt")
(require "../Lib/xsend.rkt") (time-out-limit 1.2)

;; ---------------------------------------------------------------------------------------------------
(define ((tournament-manager in out) player)
  (define name (get-field name player))
  (parameterize ([current-input-port in] [current-output-port out])
    ;; register the player with the server-side tournament manager 
    (send-message name out)
    
    ;; deal with all game interactions from, and back to, the server-side referee
    (define loop-on #false)
    (let loop ()
      
      (define-syntax-rule (ssend method ->return loop?)
        (lambda (x)
          ;; --- this is where I need to check for 3 results so we can log errors in protocol/contract
          ;; --- NOTE this is a protection against the srver; all Remotes are on the "same side"
          
          (match (xsend player method #:thrown vector #:timed-out vector x)
            [(vector)     (error 'manager "the ~a method timed out\n" 'method)]
            [(vector msg) (error 'manager "the server violated the game protocol\n ~a" msg)]
            [r            (when ->return (send-message (->return r)))
                          (or loop? (loop))])))

      (define message (read-message in))
      (cond
        [(eof-object? message) (error 'manager "the server unexpectedly closed the connection")]
        [(jsexpr->as message)            => (ssend playing-as  #false         loop-on)]
        [(and (string? message) message) => (ssend other-name  #false         loop-on)]
        [(jsexpr->placements message)    => (ssend placement   place->jsexpr  loop-on)]
        [(jsexpr->board message)         => (ssend take-turn   action->jsexpr loop-on)]
        [(jsexpr->results message)       => (ssend end-of-game #false         message)]
        [else (error 'manager "the server sent an unexpected message: ~e" message)]))))

;; ---------------------------------------------------------------------------------------------------
(module* test-support #f
  (provide
   ;; InputPort OutputPort -> Void
   test-suite)

  ;; -------------------------------------------------------------------------------------------------
  (require rackunit)
  (require json)
  (require (submod "../Common/board.rkt" test-support))
  (require "../Player/player.rkt")
  (require "../Lib/with-output-to-dev-null.rkt")
  (require "../Lib/xsend.rkt")

  ;; -------------------------------------------------------------------------------------------------
  (define (jsexpr->string ->jsexpr x)
    (with-output-to-string (lambda () (define y (->jsexpr x)) (if (jsexpr? y) (send-message y) y))))

  (define (test-suite tournament-manager)
    
    (define-syntax check-manager
      (syntax-rules ()
        [(_ rm0 sm0 msg)
         (aux-manager ([rm rm0][sm sm0][game (make-game rm)][win '(("matthias" "christos"))])
                      (check-equal? (game) `(,win ,sm #"") msg))]
        [(_ pred? rm0 sm0 msg)
         (aux-manager ([rm rm0]
                       [sm sm0]
                       [op (open-output-bytes)]
                       [game (if (regexp? pred?) (make-game rm op #:px pred?) (make-game rm op))])
                      (check-exn pred? game msg)
                      (check-equal? (get-output-bytes op) sm msg))]))

    (define-syntax-rule
      (aux-manager ([rm ((received-msg ...) ...)] [sm ((sent-msg ...) ...)] [x rhs] ...) checks ...)
      (let* ([rm (string-append (jsexpr->string received-msg ...) ...)]
             [sm (bytes-append (string->bytes/locale (jsexpr->string sent-msg ...)) ...)]
             [x  rhs] ...)
        checks ...))
    
    (define (make-game received-messages (op #false) #:px (unexpected #t))
      (lambda ()
        (define matthias (new player% [name "matthias"]))
        (define inputs   (open-input-string received-messages))
        (with-output-to-dev-null #:hide #f ;; show both stdout stderr
          (lambda ()
            (with-handlers ([(lambda (xn)
                               (and (exn:fail? xn)
                                    (not (equal? unexpected CLOSED))
                                    (regexp-match CLOSED (exn-message xn))))
                             (lambda (xn) (log-error (exn-message xn)))])
              ((tournament-manager inputs (or op (current-output-port))) matthias))))))
  
    (trailing-newline? #f)
    (define CLOSED #px"closed the connection")
  
    (check-manager
     ;; --- received messages 
     ((values "christos")
      (placements->jsexpr '())
      (placements->jsexpr '(("christos" 0 0) ("matthias" 1 1)))
      (board->jsexpr (cboard [[0christos1 2matthias1 3] [0christos2 1matthias2 2]]))
      (results->jsexpr '(("matthias" "christos"))))
     ;; --- sent messages 
     ((values "matthias")
      (place->jsexpr '(0 0))
      (place->jsexpr '(5 5))
      (action->jsexpr (winning-move (worker "matthias1") EAST PUT))
      (pretty-print '(("matthias" "christos"))))
     "testing a full run (other, placement, take-turn, and results)")

    (check-manager 
     CLOSED
     ;; --- received messages 
     ((values "christos")
      (placements->jsexpr '())
      (placements->jsexpr '(("christos" 0 0) ("matthias" 1 1))))
     ;; --- sent messages 
     ((values "matthias")
      (place->jsexpr '(0 0))
      (place->jsexpr '(5 5)))
     "testing an premature end of the conversation")

    (check-manager 
     #px"unexpected message"
     ;; --- received messages 
     ((values "christos")
      (placements->jsexpr '())
      (values '("matthias" "christos" "jared")))
     ;; --- sent messages 
     ((values "matthias")
      (place->jsexpr '(0 0)))
     "testing an unexpected JSON message")

    (check-manager 
     #px"violated the game protocol"
     ;; --- received messages 
     ((values "christos")
      (placements->jsexpr '())
      (placements->jsexpr '(("christos" 0 0))))
     ;; --- sent messages 
     ((values "matthias")
      (place->jsexpr '(0 0)))
     "testing a violation of the game protocol (wrt the placement protocol)")))
  

(module+ test
  (require (submod ".."))
  (require (submod ".." test-support))
  (test-suite tournament-manager))
