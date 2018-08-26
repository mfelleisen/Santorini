#lang racket

(require "../Common/player-interface.rkt")

(provide
 (contract-out
  (make-remote-player%
   (-> input-port? output-port? player%/c)))
 as->jsexpr
 jsexpr->as)

;; ---------------------------------------------------------------------------------------------------
(require (submod "../Common/actions.rkt" json))
(require (submod "../Common/board.rkt" json))
(require (submod "../Common/placements.rkt" json))
(require "../Lib/io.rkt") ;; see below

(module+ test
  (require "../Lib/with-output-to-dev-null.rkt")
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (make-remote-player% in out)
  (class object% (init-field name)

    (super-new)

    (define/public (playing-as my-new-name)
      (send-message (as->jsexpr my-new-name) out))

    (define/public (other name)
      (send-message name out))

    (define/public (placement lop)
      (send-message (placements->jsexpr lop) out)
      (jsexpr->place (read-message in)))
    
    (define/public (take-turn b)
      (send-message (board->jsexpr b) out)
      (jsexpr->action (read-message in)))))

(define (as->jsexpr my-new-name)
  `("playing-as" ,my-new-name))

(define (jsexpr->as message)
  (match message
    [`("playing-as" ,new-name) new-name]
    [else #false]))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  (require (submod "../Common/board.rkt" test-support))
  (require json)

  (define (jsexpr->string ->jsexpr x)
    (with-output-to-string (lambda () (define y (->jsexpr x)) (if (jsexpr? y) (send-message y) y))))
  (define-syntax-rule
    (chk-mtd (method arg) expected expected->jsexpr arg->json)
    (check-equal? (with-output-to-dev-null
                   #:hide #false
                   (lambda ()
                     (define in (open-input-string (jsexpr->string expected->jsexpr expected)))
                     (define rp (new (make-remote-player% in (current-output-port)) [name "m"]))
                     (send rp method arg)))
                  (list expected (string->bytes/locale (jsexpr->string arg->json arg)))))

  (trailing-newline? #f)
  
  (chk-mtd (other "christos") (void) void values)

  (chk-mtd (placement '())                 '(0 0) place->jsexpr placements->jsexpr)
  (chk-mtd (placement '(("christos" 0 0))) '(1 1) place->jsexpr placements->jsexpr)

  (define b0
    (cboard
     [[2christos2 2m2 3]
      [1christos1 1m1 2]]))

  (chk-mtd (take-turn b0) (giving-up "m") action->jsexpr board->jsexpr)
  (chk-mtd (take-turn b0) (winning-move (worker "m2") EAST PUT) action->jsexpr board->jsexpr)
  (chk-mtd (take-turn b0) (move-build (worker "m1") EAST PUT PUT NORTH) action->jsexpr board->jsexpr))
