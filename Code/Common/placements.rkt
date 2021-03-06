#lang racket

(provide
 placements/c
 place/c)

;; also provides a json submodule for serializing and deserializing these values

;; ---------------------------------------------------------------------------------------------------
(require "board.rkt")
(require "workers.rkt")

;; ---------------------------------------------------------------------------------------------------
(define placements/c
  (listof
   (list/c
    string?   ;; who placed which worker on the board
    in-range? ;; at x 
    in-range? ;; at y on the initial board
    )))

(define place/c (list/c in-range? in-range?))

;; ---------------------------------------------------------------------------------------------------
(module* json #f
  (provide
   placements->jsexpr
   jsexpr->placements

   place->jsexpr
   jsexpr->place)

  (define (placements->jsexpr p)
    (let loop ((p p) (seen '()))
      (cond
        [(empty? p) '()]
        [else
         (match (first p)
           [`(,(and name (? good-player-name?)) ,x ,y)
            (if (member name seen)
                (cons `(,(format "~a~a" name 2) ,x ,y) (loop (rest p) seen))
                (cons `(,(format "~a~a" name 1) ,x ,y) (loop (rest p) (cons name seen))))]
           [else (error 'placements->jsexpr "can't happen: ~e" (first p))])])))


  (define (jsexpr->placements j*)
    (let/ec return 
      (unless (list? j*) (return #false))
      (for/list ((j j*))
        (match j
          [`(,(? good-worker-name? name) ,(? natural-number/c x) ,(? natural-number/c y))
           `(,(worker-name (worker name)) ,x ,y)]
          [else (return #false)]))))

  (define (place->jsexpr p) p)

  (define (jsexpr->place j)
    (match j
      [`(,(? number?) ,(? number?)) j]
      [else #false])))
        
;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".." json))
  (require json)
  (require rackunit)

  (check-pred jsexpr? (placements->jsexpr '(("x" 1 1))))
  (check-pred jsexpr? (place->jsexpr '(1 1)))

  (define 2pieces '(("x" 1 1) ("y" 2 2)))
  (define 3pieces '(("x" 1 1) ("y" 2 2) ("x" 3 3)))
  (check-equal? (jsexpr->placements '()) '())
  (check-equal? (jsexpr->placements (placements->jsexpr 2pieces)) 2pieces)
  (check-equal? (jsexpr->placements (placements->jsexpr 3pieces)) 3pieces)
  
  (check-false  (jsexpr->placements '((0christos1 2matthias1 3) (0christos2 1matthias2 2))))
  (check-false  (jsexpr->placements "the end"))

  (define pl '(3 3))
  (check-equal? (jsexpr->place (place->jsexpr pl)) pl)
  (check-false  (jsexpr->place '("a" 1))))
  
