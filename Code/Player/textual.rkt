#lang racket

;; a player class that uses the a 2-move look-ahead strategy

(require "../Common/player-interface.rkt")

(provide
 (contract-out 
  (textual% player-protocol%/c)))
 
;; ---------------------------------------------------------------------------------------------------
(require "super.rkt")
(require "strategy.rkt")
(module+ test
  (require (for-syntax "../Common/actions.rkt"))
  (require (submod "../Common/board.rkt" test-support))
  (require "../Lib/with-output-to-dev-null.rkt")
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define textual%
  (class super% 
    (super-new)

    (inherit-field name other)

    (define/override (placement list-of-places)
      (displayln `(place the next worker))
      (displayln `(placements so far ,list-of-places))
      (let loop ()
        (displayln `(expected input (x y)))
        (define proposed (read))
        (match proposed
          [(? eof-object?) (error 'placement "unexpected end of file")]
          [`(,(? number? x) ,(? number? y))
           (cond
             [(and (in-range? x) (in-range? y)) `(,x ,y)]
             [else (displayln `(numbers out of range)) (loop)])]
          [else (loop)])))

    (define/override (take-turn board)
      (displayln `(,other took a turn))
      (displayln board)
      (let loop ()
        (displayln `(choose your next move ,name))
        (displayln `(g for giving up))
        (displayln `((w w dx dy) for winning move in directions dx and dy))
        (displayln `((m dx dy ddx ddy) for move and build))
        (define choice (read))
        (match choice
          [(? eof-object?) (error 'take-turn "unexpected end of file")]
          ['g (giving-up name)]
          [`(w ,(? 1-or-2? w) ,(? number? dx) ,(? number? dy))
           (cond ;; abstraction leakage here 
             [(and (direction/c dx) (direction/c dy) (not (and (equal? dx PUT) (equal? dx dy))))
              (winning-move (worker (string-append name (number->string w))) dx dy)]
             [else (displayln `(bad winning-move specification)) (loop)])
           ]
          [`(m ,(? 1-or-2? w) ,(? number? dx) ,(? number? dy) ,(? number? ddx) ,(? number? ddy))
           (cond
             [(and (direction/c dx) (direction/c dy) (direction/c ddx) (direction/c ddy)
                   (not (and (equal? dx PUT) (equal? dx dy)))
                   (not (and (equal? ddx PUT) (equal? ddx ddy))))
              (move-build (worker (string-append name (number->string w))) dx dy ddx ddy)]
             [else (displayln `(bad move-build specification)) (loop)])]
          [else (loop)])))))

(define (1-or-2? x)
  (and (number? x) (or (= x 1) (= x 2))))

;; -------------------------------------------------------------------------------------------------

(module+ test
  (require (submod ".."))

  (define-syntax (check-textual stx)
    (syntax-case stx (XN)
      [(_ XN (method arg) input expected msg)
       #'(check-exn
          expected
          (let* ([me (new textual% [name "x"][other "o"])])
            (lambda ()
              (with-input-from-string input
                (lambda ()
                  (with-output-to-dev-null
                   (lambda ()
                     (send me method arg)))))))
          msg)]
      [(_ (method arg) input expected msg)
       #'(check-equal?
          (let* ([me (new textual% [name "x"][other "o"])])
            (with-input-from-string input
              (lambda ()
                (with-output-to-dev-null
                 (lambda ()
                   (send me method arg))))))
            expected
            msg)]))

  (check-textual (placement '()) "(0 0)" '(0 0) "basic placement")
  (check-textual (placement '()) "(-1 0) (0 0)" '(0 0) "basic placement, loop1")
  (check-textual (placement '()) "-1 0 (0 0)" '(0 0) "basic placement, loop2")

  (check-textual XN (placement '()) "0" #px"unexpected end of file" "placement exn")

  (define b
    (cboard
     [[0x1 0 0 0 0]
      [0x2 0 0 0 0]
      [0   0 0 0 0]
      [0   0 0 0 0]
      [0   0 0 0o1 0o2]]))

  (define w1 (worker "x1"))

  (check-textual XN (take-turn b) "x" #px"unexpected end of file" "basic giving up")
  (check-textual (take-turn b) "g" (giving-up "x") "basic giving up")
  (check-textual (take-turn b) "(w 1 +1 -1)" (winning-move w1 +1 -1) "w1")
  (check-textual (take-turn b) "(w 0 +0 -0) (w 0 +0 -0) (w 1 +1 -1)" (winning-move w1 +1 -1) "w*1")
  (check-textual (take-turn b) "(w 1 +0 -0) (w 0 +0 -0) (w 1 +1 -1)" (winning-move w1 +1 -1) "w*A")
  (check-textual (take-turn b) "(w 1 +1 -1) (w 0 +2 -1) (w 1 +1 -1)" (winning-move w1 +1 -1) "w*B")
  (check-textual (take-turn b) "(w 1 +1 -2) (w 0 +2 -1) (w 1 +1 -1)" (winning-move w1 +1 -1) "w*2")
  (check-textual (take-turn b) "(m 1 +1 -1 0 1)" (move-build w1 +1 -1 0 1) "m1")
  (check-textual (take-turn b) "(m 1 +1 -1 0 0) (m 1 +1 -1 0 1)" (move-build w1 +1 -1 0 1) "m*1")
  (check-textual (take-turn b) "(m 1 0 0 1 1)   (m 1 +1 -1 0 1)" (move-build w1 +1 -1 0 1) "m*2")
  (check-textual (take-turn b) "(m 0 0 0 1 1)   (m 1 +1 -1 0 1)" (move-build w1 +1 -1 0 1) "m*3")
  (check-textual (take-turn b) "(m 1 a 0 1 1)   (m 1 +1 -1 0 1)" (move-build w1 +1 -1 0 1) "m*4"))
