#lang racket

(require "../Common/player-interface.rkt")

(provide
 (contract-out
  (make-remote-player%
   (-> input-port? output-port? player%/c))))

;; ---------------------------------------------------------------------------------------------------
(require (submod "../Common/actions.rkt" json))
(require (submod "../Common/board.rkt" json))
(require (submod "../Common/placements.rkt" json))
; (require "../Lib/common.rkt") ;; see below 

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (make-remote-player% in out)
  (class object% (init-field name)

    (super-new)

    (define/public (other name)
      (send-message name out))

    (define/public (placement lop)
      (send-message (placements->jsexpr lop) out)
      (jsexpr->place (read-message in)))
    
    (define/public (take-turn b)
      (send-message (board->jsexpr b) out)
      (jsexpr->action (read-message in)))))

;; ---------------------------------------------------------------------------------------------------
;; wish list (done)

(module tcp racket
  (provide
   (contract-out
    (send-message    (-> jsexpr? output-port? void))
    (read-message (-> input-port? jsexpr?))))

  (require json)

  (define (send-message j out)
    (write-json j out)
    (flush-output out))

  (define (read-message in)
    (read-json in)))
(require 'tcp)
  
;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  (require (submod "../Common/board.rkt" test))
  (require json)

  (define (jsexpr->string ->jsexpr x)
    (with-output-to-string (lambda () (define y (->jsexpr x)) (if (jsexpr? y) (write-json y) y))))
  (define-syntax-rule
    (chk-mtd (method arg) expected expected->jsexpr arg->json)
    (check-equal? (let ()
                    (define response (jsexpr->string expected->jsexpr expected))
                    (define in  (open-input-string response))
                    (define out (open-output-string))
                    ;; - - - 
                    (define rp% (make-remote-player% in out))
                    (define rp  (new rp% [name "m"]))
                    ;; - - - 
                    (define actual-result  (send rp method arg))
                    (define actual-message (get-output-string out))
                    (list actual-result actual-message))
                  (let () 
                    (list expected (jsexpr->string arg->json arg)))))
  

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


