#lang racket

(provide

 #;((All (X) ([Listof X] [X -> [X -> Boolean]] [Setof X])))
 ;; eliminate all but the first x from lox that have porperty-x?
 ;; and create a set from the result 
 set-from)

;; -----------------------------------------------------------------------------
(module+ test (require rackunit))

;; -----------------------------------------------------------------------------
(define (set-from lox0 property-x?)
  (define clean-lox
    (let loop ((lox lox0))
      (cond
        [(empty? lox) '()]
        [else (define fst (first lox))
              (define rst (remf* (property-x? fst) lox))
              (cons fst (loop rst))])))
  (apply set clean-lox))

;; -----------------------------------------------------------------------------
(module+ test
  (struct posn (x y) #:transparent)

  (check-equal? (set-from (list (posn 0 0) (posn 1 1) (posn 0 0))
                          (lambda (p)
                            (match-define (posn x y) p)
                            (lambda (q)
                              (match-define (posn x1 y1) q)
                              (and (= x x1) (= y y1)))))
                (set (posn 0 0) (posn 1 1))))