#lang racket

;; The Board

;; what knowledge is turned into information here and represented with data:
;; -- where the tokens are
;; -- where buildings are
;; -- how tall buildings are
;; -- whether the board is in a "final state"
;; ---------------------------------------------------------------------------------------------------

(require (only-in "token.rkt" token? in-range? direction/c at-distinct-places))

(provide
 
 ;; type Building = (building Range Range N)
 MAX-HEIGHT ; a buidling is called 'capped' if its MAX-HEIGHT stories tall. 
 TOP-FLOOR  ; this is the victory story 

 ;; type board
 board?

 ;; Token Token Token Token -> Board
 board

 (contract-out 
  (init
   ;; create the board and place the four tokens on it
   (->i ((t1 token?) (t2 token?) (t3 token?) (t4 token?))
        #:pre (t1 t2 t3 t4) (at-distinct-places (list t1 t2 t3 t4))
        (r board?)))
 
  (on-board?
   ;; does this token exist on the current board?
   (-> board? (-> token? boolean?)))

  (stay-on-board?
   ;; does this token stay on baord if it moves in the specified direction?
   ;; (this is called only when on-board? has been confirmed)
   (-> board? token? direction/c direction/c boolean?))
 
  (height-of
   (-> board? in-range? in-range? natural-number/c))
 
  (location-free-of-token?
   ;; there is no token on (x,y)
   (-> board? in-range? in-range? boolean?))
  
  (move
   ;; move the token one step in the given direction
   ;; (move will be called from admin only; no checks needed to ensure legality of move)
   (->i ((b board?) (t (b) (and/c token? (on-board? b))) (e-w direction/c) (n-s direction/c))
        (r board?)))
 
  (build
   ;; add a level to the buidling that is in the specified direction
   ;; (move will be called from admin only; no checks needed to ensure legality of build
   (->i ((b board?) (t (b) (and/c token? (on-board? b))) (e-w direction/c) (n-s direction/c))
        (r board?)))
 
  (the-end?
   ;; is any three-story building occupied by a token?
   (-> board? boolean?))))

;; ---------------------------------------------------------------------------------------------------
;; DEPENDENCIES

(require (except-in "token.rkt" token? in-range? direction/c at-distinct-places))
(require "../Lib/struct-with.rkt")

(require (for-syntax syntax/parse))
(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION  

(struct building (x y height) #:transparent)
(define TOP-FLOOR  3)
(define MAX-HEIGHT 4)

(struct board (tokens buildings)
  #:transparent
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

(define (init token1 token2 token3 token4)
  (board (list token1 token2 token3 token4) '()))

(define ((on-board? b) t)
  (with board b (cons? (member t tokens))))

(define (stay-on-board? b t e-w n-s)
  (with board b (with token t (and (in-range? (+ x e-w)) (in-range? (+ y n-s))))))

(define (height-of b x y)
  (with board b
        (define is-there-a-building (find-building buildings x y))
        (if (boolean? is-there-a-building)
            0
            (building-height is-there-a-building))))

(define (location-free-of-token? b x0 y0)
  (with board b
        (ormap (lambda (t) (with token t (not (and (= x0 x) (= y0 y))))) tokens)))

(define (move b token e-w n-s)
  (with board b
        (board (replace (move-token token n-s e-w) tokens) buildings)))

(define (build b token e-w n-s)
  (with board b
        (define-values (x-where-to-build y-where-to-build) (neighbor-location token n-s e-w))
        (define is-there-a-building (find-building buildings x-where-to-build y-where-to-build))
        (define the-building (or is-there-a-building (building x-where-to-build y-where-to-build 0)))
        (board tokens (replace (build-on the-building) buildings))))

(define (the-end? b)
  (with board b
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
  ; (require syntax/parse)
  (require (for-syntax racket/list))

  (begin-for-syntax
    (define CELL #px"(\\d*)([a-z])")
  
    (define-syntax-class cell
      (pattern x:id
               #:do [(define sym (syntax-e #'x))
                     (define str (symbol->string sym))
                     (define mat (regexp-match CELL str))]
               #:fail-unless (and mat (string->number (second mat))) "not a cell spec"
               #:attr v #`(list #,(string->number (second mat)) #,(third mat)))
      (pattern x:integer #:attr v #'x))

    (define-syntax-class literal-board
      [pattern [[x:cell ...] ...]
               #:do [(define x-board #'(list (list x.v ...) ...))]
               #:fail-unless x-board "not a board"
               #:attr board (datum->syntax #'here x-board)]))

  (define-syntax (board-check stx)
    (syntax-parse stx
      [(_ b:literal-board (tx:integer ty:integer) arg ... proc:id (result))
       #'(check-equal?
          (proc (->board b.board) (token "x" tx ty) arg ...)
          result)]))

  ;; [Listof [Listof (U Integer [List Integer Letter])]] -> (U Board  #false)
  (define (->board x)
    (define (+token accu cell x y) (if (integer? cell) accu (cons (token (second cell) x y) accu)))
    (define tokens (traverse-literal-board x +token))
    (define (+building accu cell x y)
      (if (integer? cell)
          (cons (building x y cell) accu)
          (cons (building x y (first cell)) accu)))
    (define buildings (traverse-literal-board x +building))
    (and tokens buildings (2-tokens tokens) (board tokens buildings)))

  ;; [Listof Token] -> Boolean
  ;; there are exactly four tokens and only two kinds of tokens 
  (define (2-tokens tokens)
    (define names (map (lambda (t) (with token t color)) tokens))
    (and (= (length names) 4)
         (let ([names-first (remove* (list (first names)) names)])
           (and (= (length names-first) 2)
                (let ([names-other (remove* (list (first names-first)) names-first)])
                  (empty? names-other))))))

  ;; (All X) [Listof [Listof Any]] [(list Any N Letter) N N -> X] -> (U [Listof X] #false)
  (define (traverse-literal-board x f)
    (let/ec return
      (for/fold ([tokens '()]) ([row x][row# (in-naturals)])
        (for/fold ([tokens tokens]) ([cell row][col# (in-naturals)])
          (f tokens cell row# col#))))))

(module+ test 
  (check-equal? (init 't1 't2 't3 't4) (board (list 't1 't2 't3 't4) '()))

  (define (foo b t x y) b)
  (board-check
   [[3 2x 1x]
    [3 2o 1o]]
   (0 1)
   2 3
   foo
   [(board
     (list (token "o" 1 2) (token "o" 1 1) (token "x" 0 2) (token "x" 0 1))
     (list (building 1 2 1) (building 1 1 2) (building 1 0 3)
           (building 0 2 1) (building 0 1 2) (building 0 0 3)))]))
