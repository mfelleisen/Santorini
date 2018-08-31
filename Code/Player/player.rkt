#lang racket

(require "../Common/player-interface.rkt")

(provide
 (contract-out 
  (player% player-protocol%/c)))
 
;; ---------------------------------------------------------------------------------------------------
(require "strategy.rkt")
(module+ test
  (require (submod "../Common/board.rkt" test-support))
  (require "../Lib/with-output-to-dev-null.rkt")
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define player%
  (class object% (init-field name (other "aaaaxxxx"))
    (super-new)

    (field
      [playing-as-has-been-called-once #false]
      [other-name-has-been-called      #false]
      [placement-has-been-called-once  #false]
      [placement-has-been-called-twice #false])

    (define strategy #f)

    (define/public (playing-as my-new-name)
      (set! name my-new-name))

    (define/public (other-name oname)
      (set! other oname)
      (set! strategy (new strategy% [player name][other oname])))

    (define/public (placement list-of-places)
      (send strategy initialization list-of-places))
    
    (define/public (take-turn board)
      (send strategy take-turn board))
    
   (define/public (end-of-game results)
     (pretty-print results))))

 ;; -------------------------------------------------------------------------------------------------

(module+ test
  (require (submod ".."))

  (define (make-safe x o)
    (define p (new player% [name x]))
    (send p playing-as x) 
    (send p other-name o) p)

  ;; end of game
  (check-equal?
   (with-output-to-string (lambda () (send (make-safe "x" "o") end-of-game '(("x" "o")))))
   "'((\"x\" \"o\"))\n")


  ;; initialization 
  (check-pred cons? (send (make-safe "x" "o") placement '()) "the mechanics work out")
  
  (define x-o-safe (make-safe "x" "o"))
  (check-equal? (send x-o-safe placement '()) (list 0 0))
  (check-equal? (send x-o-safe placement `(("x" 1 1)("o" 0 0))) (list 5 5))

  (define x-o-snd-call-safe (make-safe "x" "o"))
  (check-equal? (begin
                  (send x-o-snd-call-safe placement '())
                  (send x-o-snd-call-safe placement `(("o" 0 0)("x" 5 5))))
                (list 4 5))

  (define o-x--snd-call-safe (make-safe "o" "x"))
  (check-equal? (begin
                  (send o-x--snd-call-safe placement '())
                  (send o-x--snd-call-safe placement `(("x" 4 5)("o" 0 0)("x" 5 5))))
                (list 1 0))

  (define-syntax-rule
    (check-protocol lop1 lop2 ... msg)
    (check-exn exn:fail:contract?
               (lambda ()
                 (with-output-to-dev-null ; #:error-port #true
                     (lambda ()
                       (define x-o (make-safe "x" "o"))
                       (send x-o placement 'lop1)
                       (send x-o placement 'lop2) ...)))
               msg))

  (check-protocol (("x" 1 1)) "first call, but already placed a token")
  (check-protocol (("o" 1 1)) (("o" 1 1)) "second call, 'I' have not placed a token yet")
  (check-protocol (("o" 1 1)) (("x" 1 1)) "second call, 'other' has not placed a token yet"))
