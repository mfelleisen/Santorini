#lang racket

;; what knowledge is turned into information here and represented with data:
;; -- where the tokens are
;; -- where buildings are
;; -- how tall buildings are
;; -- whether the board is in a "final state"
;; ---------------------------------------------------------------------------------------------------

(provide
 
 ;; type Building = (building Range Range N)

 ;; Token Token Token Token -> Board
 ;; create the board and place the four tokens on it
 ;; ASSUME the tokens occupy four distinct places 
 init

 ;; Board -> [Listof Building]
 board-buildings

 ;; Board -> [Listof Token]
 board-tokens 

 ;; Board Token Direction Direction -> Board
 ;; move the token one step in the given direction
 ;; ASSUME the token is on the board 
 move

 ;; Board Token Direction Direction -> Board
 ;; add a level to the buidling that is in the specified direction
 ;; ASSUME the token is on the board 
 build

 ;; Board -> Boolean
 ;; is any three-story building occupied by a token?
 the-end?)

;; ---------------------------------------------------------------------------------------------------
;; DEPENDENCIES

(require "tokens.rkt")
(require "Lib/struct-with.rkt")

(require (for-syntax syntax/parse))
(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION  

(struct-with board (tokens buildings)
             #:methods gen:equal+hash
             [(define (equal-proc b1 b2 equal?)
                (match-define (board b1t b1b) b1)
                (match-define (board b2t b2b) b2)
                (and (equal? b1t b2t) (equal? b1b b2b)))
              (define (hash-proc bd rhash)
                (match-define (board t b) bd)
                (+ (* 10 (length t)) (length b)))
              (define (hash2-proc bd rhash2)
                (match-define (board t b) bd)
                (+ (* 100 (length t)) (* 10 (length b))))])

(struct-with building (x y height) #:transparent)

(define (init token1 token2 token3 token4)
  (board (list token1 token2 token3 token4) '()))

(define (move b token e-w n-s)
  (with-board b
    (board (replace (move-token token n-s e-w) tokens) buildings)))

(define (build b token e-w n-s)
  (with-board b
    (define-values (x-where-to-build y-where-to-build) (position-of token n-s e-w))
    (define is-there-a-building (find-building buildings x-where-to-build y-where-to-build))
    (define the-building (or is-there-a-building (building x-where-to-build y-where-to-build 0)))
    (board tokens (replace (build-on the-building) buildings))))

(define (the-end? b)
  (with-board b
    (define 3-levels  (filter (lambda (b) (= (building-height b) 3)) buildings))
    (define occupied? (filter (lambda (t) (ormap (on? t) 3-levels)) tokens))
    (cons? occupied?)))

;; [Listof Building] Range Range -> (U Building #false)
(define (find-building buildings x-where-to-build y-where-to-build)
  (ormap (lambda (b)
           (match-define (building x y _) b)
           (and (equal? x x-where-to-build) (equal? y y-where-to-build)))
         buildings))

;; Token -> [Building -> Boolean]
;; ie the token in the same position as the building?
(define ((on? t) b)
  (define-values (tx ty) (token-location t))
  (match-define  (building bx by _) b)
  (and (equal? tx bx) (equal? ty by)))

;; X [Listof X] -> [Listof X]
;; "replace" an x-es on lox with the same location by x
(define (replace x lox)
  (cons x lox))

;; Building -> Building
(define (build-on b)
  (match-define (building x y z) b)
  (building x y (+ z 1)))

;; ---------------------------------------------------------------------------------------------------
;; TESTS
(module+ test
  (check-equal? (init 't1 't2 't3 't4) (board (list 't1 't2 't3 't4) '())))
