#lang racket

(require "../Common/player-interface.rkt")

(provide
 (contract-out
  (make-remote-player%
   (-> input-port? output-port? player-protocol%/c)))
 as->jsexpr
 jsexpr->as)

;; ---------------------------------------------------------------------------------------------------
(require "../Player/super.rkt")
(require (submod "../Common/actions.rkt" json))
(require (submod "../Common/board.rkt" json))
(require (submod "../Common/placements.rkt" json))
(require (submod "../Common/results.rkt" json))
(require "../Lib/io.rkt") ;; see below

(module+ test
  (require "../Lib/with-output-to-dev-null.rkt")
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (make-remote-player% in out)

  (define-syntax define/remote 
    (syntax-rules (augment)
      [(_ (method ->to augment))
       (define/augment (method arg) (send-message (->to arg) out))]
      [(_ (method ->to))
       (define/override (method arg) (send-message (->to arg) out))]
      [(_ (method ->to <-from))
       (define/override (method arg)
         (send-message (->to arg) out)
         (<-from (read-message in)))]))
  
  (class super%
    (super-new (other "aaxxxx"))
    (define/remote (playing-as as->jsexpr))
    (define/remote (other-name values augment))
    (define/remote (placement placements->jsexpr jsexpr->place))
    (define/remote (take-turn board->jsexpr jsexpr->action))
    (define/remote (end-of-game results->jsexpr))))

(define (as->jsexpr my-new-name)
  `("playing-as" ,my-new-name))

(define (jsexpr->as message)
  (match message
    [`("playing-as" ,(and new-name (? good-player-name?))) new-name]
    [else #false]))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  (require (submod "../Common/board.rkt" test-support))
  (require json)

  (define matthias1 "matthias")
  (define matthias2 "matthias2")
  (check-pred jsexpr? (as->jsexpr matthias1))
  (check-equal? (jsexpr->as (as->jsexpr matthias1)) matthias1)
  (check-false (jsexpr->as matthias2) matthias2)
  (check-false (jsexpr->as '["playing-as" ""]))

  (define (jsexpr->string ->jsexpr x)
    (with-output-to-string (lambda () (define y (->jsexpr x)) (if (jsexpr? y) (send-message y) y))))
  (define-syntax-rule
    (chk-mtd (method arg) expected expected->jsexpr arg->jsexpr)
    (check-equal? (with-output-to-dev-null
                   #:hide #false
                   #:error-port (open-output-string)
                   (lambda ()
                     (define in (open-input-string (jsexpr->string expected->jsexpr expected)))
                     (define rp (new (make-remote-player% in (current-output-port)) [name "m"]))
                     (send rp method arg)))
                  (list expected (string->bytes/locale (jsexpr->string arg->jsexpr arg)))))

  (trailing-newline? #f)

  (chk-mtd (playing-as "christos2") (void) as->jsexpr as->jsexpr)
  
  (chk-mtd (other-name "christos") (void) void values)

  (chk-mtd (placement '())                 '(0 0) place->jsexpr placements->jsexpr)
  (chk-mtd (placement '(("christos" 0 0))) '(1 1) place->jsexpr placements->jsexpr)

  (define b0
    (cboard
     [[2christos2 2m2 3]
      [1christos1 1m1 2]]))

  (chk-mtd (take-turn b0) (giving-up "m") action->jsexpr board->jsexpr)
  (chk-mtd (take-turn b0) (winning-move (worker "m2") EAST PUT) action->jsexpr board->jsexpr)
  (chk-mtd (take-turn b0) (move-build (worker "m1") EAST PUT PUT NORTH) action->jsexpr board->jsexpr))
