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
(require (submod "../Common/results.rkt" json))
(require "../Lib/io.rkt") ;; see below

(module+ test
  (require "../Lib/with-output-to-dev-null.rkt")
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (make-remote-player% in out)
  (class object% (init-field name (other "aaxxxx"))

    (super-new)

    (field
      [playing-as-has-been-called-once #false]
      [other-name-has-been-called      #false]
      [placement-has-been-called-once  #false]
      [placement-has-been-called-twice #false])

    (define/public (playing-as my-new-name)
      (send-message (as->jsexpr my-new-name) out))

    (define/public (other-name name)
      (send-message name out))

    (define/public (placement lop)
      (send-message (placements->jsexpr lop) out)
      (jsexpr->place (read-message in)))
    
    (define/public (take-turn b)
      (send-message (board->jsexpr b) out)
      (jsexpr->action (read-message in)))

    (define/public (end-of-game results)
      (send-message (results->jsexpr results) out))))

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

  (define matthias2 "matthias2")
  (check-pred jsexpr? (as->jsexpr matthias2))
  (check-equal? (jsexpr->as (as->jsexpr matthias2)) matthias2)
  (check-false (jsexpr->as matthias2) matthias2)

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
