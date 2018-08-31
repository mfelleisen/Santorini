#lang racket

(require "../Common/observer-interface.rkt")

(provide
 (contract-out 
  (textual-observer% observer%/c)))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  ; (require (submod ".."))
  (require "../Common/actions.rkt")
  (require "../Common/directions.rkt")
  (require (submod "../Common/board.rkt" test-support))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define textual-observer%
  (class object%
    (super-new)
    (define/public (action a)
      (displayln a))
    (define/public (board b)
      (display b))
    (define/public (report msg)
      (displayln msg))))

;; -----------------------------------------------------------------------------
(module+ test
  (define observer (new textual-observer%))

  (define-syntax-rule
    (check-method (method arg) expected msg)
    (check-equal? (with-output-to-string (lambda () (send observer method arg)))
                  (string-append expected "\n")
                  msg))

  
  (check-method (action (giving-up "me")) "(me is giving up)" "act up")
  (check-method (action (winning-move (worker "me1") EAST PUT))
                "(me1 requests to move EAST and PUT and claims this move is a winner)"
                "act win")
  (check-method (action (move-build (worker "me1") EAST SOUTH WEST NORTH))
                "(me1 requests to move EAST and SOUTH and then to build to its WEST and NORTH)"
                "act move-build")

  (check-method (board (cboard [[1x1 2x2][3o1 0o2]])) "[[1x1 2x2]\n [3o1 0o2]]" "board")

  (check-method (report "you won") "you won" "report"))