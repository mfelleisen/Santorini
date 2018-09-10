#lang racket

;; a player class that uses the a 2-move look-ahead strategy

(require "../Common/player-interface.rkt")

(provide
 (contract-out 
  (textual% player-protocol%/c)))
 
;; ---------------------------------------------------------------------------------------------------
(require "super.rkt")
(require (rename-in "../Common/directions.rkt" (east-west/c ew?) (north-south/c ns?)))
(module+ test
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
          [`(w ,(? 1-or-2? w) ,(or (? ew? dx)  (? sew? dx)) ,(or (? ns? dy) (? sns? dy)))
           (define dx0 (->ew dx)) (define dy0 (->ns dy))
           (if (staying-put? dx0 dy0)
               (begin (displayln `(bad winning-move specification)) (loop))
               (winning-move (worker (string-append name (number->string w))) dx0 dy0))]
          [`(m ,(? 1-or-2? w)
               ,(or (? ew? dx)  (? sew? dx))  ,(or (? ns? dy) (? sns? dy))
               ,(or (? ew? ddx) (? sew? ddx)) ,(or (? ns? ddy) (? sns? ddy)))
           (define dx0 (->ew dx))   (define dy0 (->ns dy))
           (define ddx0 (->ew ddx)) (define ddy0 (->ns ddy))
           (if (or (staying-put? dx0 dy0) (staying-put? ddx0 ddy0))
               [begin (displayln `(bad move-build specification)) (loop)]
               (move-build (worker (string-append name (number->string w))) dx0 dy0 ddx0 ddy0))]
          [else (loop)])))))

(define (1-or-2? x) (and (number? x) (or (= x 1) (= x 2))))
(define (staying-put? x y) (and (equal? x PUT) (equal? x y)))

(define EW `((EAST ,EAST) (PUT ,PUT) (WEST ,WEST)))
(define (sew? x) (assoc x EW))
(define (->ew x) (or (and (number? x) x) (second (assoc x EW))))

(define NS `((NORTH ,NORTH) (PUT ,PUT) (SOUTH ,SOUTH)))
(define (sns? x) (assoc x NS))
(define (->ns x) (or (and (number? x) x) (second (assoc x NS))))

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
  (check-textual (take-turn b) "(w 1 EAST -1)" (winning-move w1 +1 -1) "w1")
  (check-textual (take-turn b) "(w 1 EAST NORTH)" (winning-move w1 +1 -1) "w1")
  (check-textual (take-turn b) "(w 0 +0 -0) (w 0 +0 -0) (w 1 +1 -1)" (winning-move w1 +1 -1) "w*1")
  (check-textual (take-turn b) "(w 1 +0 -0) (w 0 +0 -0) (w 1 +1 -1)" (winning-move w1 +1 -1) "w*A")
  (check-textual (take-turn b) "(w 1 +1 -1) (w 0 +2 -1) (w 1 +1 -1)" (winning-move w1 +1 -1) "w*B")
  (check-textual (take-turn b) "(w 1 +1 -2) (w 0 +2 -1) (w 1 +1 -1)" (winning-move w1 +1 -1) "w*2")
  (check-textual (take-turn b) "(m 1 +1 -1 0 1)" (move-build w1 +1 -1 0 1) "m1")
  (check-textual (take-turn b) "(m 1 EAST -1 0 1)" (move-build w1 +1 -1 0 1) "m1")
  (check-textual (take-turn b) "(m 1 EAST NORTH PUT 1)" (move-build w1 +1 -1 0 1) "m1")
  (check-textual (take-turn b) "(m 1 EAST -1 PUT SOUTH)" (move-build w1 +1 -1 0 1) "m1")
  (check-textual (take-turn b) "(m 1 +1 -1 0 0) (m 1 +1 -1 0 1)" (move-build w1 +1 -1 0 1) "m*1")
  (check-textual (take-turn b) "(m 1 0 0 1 1)   (m 1 +1 -1 0 1)" (move-build w1 +1 -1 0 1) "m*2")
  (check-textual (take-turn b) "(m 0 0 0 1 1)   (m 1 +1 -1 0 1)" (move-build w1 +1 -1 0 1) "m*3")
  (check-textual (take-turn b) "(m 1 a 0 1 1)   (m 1 +1 -1 0 1)" (move-build w1 +1 -1 0 1) "m*4"))
