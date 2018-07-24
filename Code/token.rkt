#lang racket

;; what knowledge is turned into information here and represented with data:
;; -- a token represents a position on the board 
;; -- a direction tells me where the token goes 
;; -- .. or direction a token eyes for adding a level to a building 

;; ---------------------------------------------------------------------------------------------------

(define DIM 5)
(define in-range? (integer-in 0 DIM))

(define NORTH -1)
(define SOUTH +1)
(define PUT    0)
(define EAST  +1)
(define WEST  -1)
(define direction/c (or/c NORTH SOUTH PUT EAST WEST))

(provide
 DIM
 ;; type Range = [0,DIM)

 ;; Int -> Boolean
 in-range? 

 NORTH
 SOUTH
 PUT
 EAST
 WEST

 ;; Any -> Boolean
 ;; is this a linear direction 
 direction/c

 ;; type Token = (token String Range Range)
 token?

 ;; [Listof Token] -> {Setof Token]
 first2
 
 (contract-out

  (token
   (-> string? in-range? in-range? token?))
  
  (token-location
   (-> token? (values in-range? in-range?)))
  
  (neighbor-location
   (-> token? direction/c direction/c (values in-range? in-range?)))

  (move-token
   (-> token? direction/c direction/c token?))
  
  (at-distinct-places
   ;; are all tokens at distinct places 
   (-> (listof token?) boolean?))

  (all-directions-to-neighbors
   ;; compute all possible directions to a neighboring field from this token
   ;; GUARANTEE (0,0) is not a part of the directions 
   (-> token? (listof (list/c direction/c direction/c))))

  (exactly-2-tokens-of-2-kinds
   (-> (listof token?) boolean?))

  (stay-on-board?
   ;; does this token stay in range if it moves in the specified direction?
   ;; ASSUME token is in range 
   (-> token? direction/c direction/c boolean?))))

;; ---------------------------------------------------------------------------------------------------
(require "../Lib/struct-with.rkt")
(module+ test (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct token (color x y) #:transparent)

(define (same-token t1)
  (define name1 (token-color t1))
  (lambda (t2)
    (string=? (token-color t2) name1)))

(define (token-location t)
  (with token t (values x y)))

(define (move-token t e-w n-s)
  (define-values (x1 y1) (neighbor-location t e-w n-s))
  (token (token-color t) x1 y1))

(define (neighbor-location t e-w n-s)
  (with token t (values (+ x e-w) (+ y n-s))))

(define (all-directions-to-neighbors t)
  (with token t
        (for*/list ((e-w `(,WEST ,PUT ,EAST))
                    (n-s `(,NORTH ,PUT ,SOUTH))
                    (new-e-w (in-value (+ x e-w)))
                    (new-n-s (in-value (+ y n-s)))
                    #:when (and (not (= 0 e-w n-s)) (in-range? new-e-w) (in-range? new-n-s)))
          (list e-w n-s))))

(define (at-distinct-places lot)
  (define L (map (lambda (t) (with token t (list x y))) lot))
  (define N (length L))
  (define S (apply set L))
  (= (set-count S) N))

(define (stay-on-board? t e-w n-s)
  (with token t (and (in-range? (+ x e-w)) (in-range? (+ y n-s)))))

#; ([Listof Token] -> [Setof Token])
(define (first2 lox0)
  (define strings (set->list (apply set (map token-color lox0))))
  (let loop ((L lox0) (string1 (first strings)) (result1 #f) (string2 (second strings)) (result2 #f))
    (cond
      [(empty? L) (error "can't happen")]
      [else (define fst (first L))
            (define col (token-color fst))
            (cond
              [(string=? col string1)
               (cond
                 [(and result1 (set? result2))
                  (set-union (set fst result1) result2)]
                 [result1
                  (loop (rest L) string1 (set fst result1) string2 result2)]
                 [else
                  (loop (rest L) string1 fst string2 result2)])]
              [(string=? col string2)
               (cond
                 [(and (set? result1) result2)
                  (set-union result1 (set fst result2))]
                 [result2
                  (loop (rest L) string1 result1 string2 (set fst result2))]
                 [else
                  (loop (rest L) string1 result1 string2 fst)])]
              [else (error 'first2 "can't happen ~e" lox0)])])))

#; ([Listof Token] -> Boolean)
(define (exactly-2-tokens-of-2-kinds tokens)
  (define names (map (lambda (t) (with token t color)) tokens))
  (and (or (= (length names) 4) (error '->board "too few tokens"))
       (let* ([fst (first names)]
              [names-first (remove* (list fst) names)])
         (and (or (= (length names-first) 2) (error '->board "too few tokens of kind ~a" fst))
              (let* ([snd  (first names-first)]
                     [names-other (remove* (list snd) names-first)])
                (or (empty? names-other) (error '->board "too few tokens of kind ~a" snd)))))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  ; (require (submod "..")) ; some of these calls intentionally break contracts 

  (define O (token "christos" 0 0))
  (check-equal? (let-values ([(x y) (neighbor-location O PUT NORTH)]) (list x y)) '(0 -1))
  (check-equal? (let-values ([(x y) (neighbor-location O PUT SOUTH)]) (list x y)) '(0 +1))
  (check-equal? (let-values ([(x y) (neighbor-location O EAST PUT)]) (list x y)) '(+1 0))
  (check-equal? (let-values ([(x y) (neighbor-location O WEST PUT)]) (list x y)) '(-1 0))
  
  (check-true  (at-distinct-places (list (token 'a 1 1) (token 'b 2 2))))
  (check-false (at-distinct-places (list (token 'a 1 1) (token 'b 1 1))))

  (check-equal? (all-directions-to-neighbors O) '((0 1) (1 0) (1 1)))
  (check-equal? (all-directions-to-neighbors (token "mf" 1 0)) '((-1 0) (-1 1) (0 1) (1 0) (1 1)))

  (define lox0  (list (token "o" 2 1) (token "o" 1 1) (token "x" 2 0) (token "x" 1 0)))
  (check-equal? (first2 lox0) (set (token "o" 1 1) (token "x" 1 0) (token "o" 2 1) (token "x" 2 0))))
