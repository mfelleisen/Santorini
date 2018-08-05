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
 ; board

 (contract-out 
  (init
   ;; create the board and place the four tokens on it
   (->i ((t1 token?) (t2 token?) (t3 token?) (t4 token?))
        #:pre (t1 t2 t3 t4) (at-distinct-places (list t1 t2 t3 t4))
        (r board?)))
 
  (on-board?
   ;; does this token exist on the current board?
   (-> board? (-> token? boolean?)))
  
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
        (r board?)))))

(module+ test
  (provide
   ;; SYNTAX
   #; (define-board name [[x-y ...] ...])
   ;; defines a literal board with cells x-y ...
   ;; Each cell must either be an integer or an identifier that combines a natural with a letter.
   ;; An integer denotes a building of that height.
   ;; An identifier denotes a building of that height with a token named by the letter atop.
   ;; The grid's origin is the top left. Moving down is moving south, moving right means moving east.
   #;[[0 0 1x]
      [2y]
      [1x 3 2y]]
   ;; is a board with two "x" tokens at (2,0) and (0,2), each at height 1,
   ;; and two "y" tokens at (0,1) and (2,2), each at height 2; 
   ;; there is one other buildig at (1,2) of height 3. 
   define-board))

;; ---------------------------------------------------------------------------------------------------
;; DEPENDENCIES

(require (except-in "token.rkt" token? in-range? direction/c at-distinct-places))
(require "../Lib/set-from.rkt")
(require "../Lib/struct-with.rkt")

(require (for-syntax syntax/parse))
(module+ test
  (require rackunit)
  (require (for-syntax racket/list))
  (require syntax/macro-testing))

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION  

(struct building (x y height) #:transparent)
(define TOP-FLOOR  3)
(define MAX-HEIGHT 4)

(define (same-building b1)
  (match-define (building x1 y1 z1) b1)
  (lambda (b2)
    (match-define (building x2 y2 z2) b2)
    (and (= x1 x2) (= y1 y2))))

(struct board (tokens buildings)
  #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc b1 b2 equal?)
     (match-define (board b1t b1b) b1)
     (match-define (board b2t b2b) b2)
     (and (set=? (active-tokens b1t) (active-tokens b2t))
          (set=? (set-from b1b same-building) (set-from b2b same-building))))
   ;; the following two are, well, inappropriate in general 
   (define (hash-proc bd rhash) 0)
   (define (hash2-proc bd rhash2) 1)])

#; ([Listof Token] -> [Setof Token])
;; extract the first two tokens for each name
(define (active-tokens lox0)
  (define names (set->list (apply set (map token-name lox0))))
  #; ([Listof Token] String (U False Token [Setof Token])  String (U False Token [Setof Token]) -> ..)
  (let loop ((L lox0) (name1 (first names)) (result1 #f) (name2 (second names)) (result2 #f))
    (define fst (first L))
    (define col (token-name fst))
    (if (string=? col name1)
        (if (and result1 (set? result2)) 
            (set-union (set fst result1) result2)
            (loop (rest L) name1 (if result1 (set fst result1) fst) name2 result2))
        (if (and (set? result1) result2)
            (set-union result1 (set fst result2))
            (loop (rest L) name1 result1 name2 (if result2 (set fst result2) fst))))))

(define (init token1 token2 token3 token4)
  (board (list token1 token2 token3 token4) '()))

(define ((on-board? b) t)
  (with board b (cons? (member t tokens))))

(define (height-of b x y)
  (with board b
        (define is-there-a-building (find-building buildings x y))
        (if (boolean? is-there-a-building)
            0
            (building-height is-there-a-building))))

(define (location-free-of-token? b x0 y0)
  (for/and ((t (board-tokens b)))
    (define-values (x y) (token-location t))
    (not (and (= x0 x) (= y0 y)))))

(define (move b token e-w n-s)
  (with board b (board (replace (move-token token e-w n-s) tokens) buildings)))

(define (build b token e-w n-s)
  (with board b
        (define-values (x-where-to-build y-where-to-build) (neighbor-location token e-w n-s))
        (define is-there-a-building (find-building buildings x-where-to-build y-where-to-build))
        (define the-building (or is-there-a-building (building x-where-to-build y-where-to-build 0)))
        (board tokens (replace (build-on the-building) buildings))))

;; [Listof Building] Range Range -> (U Building #false)
(define (find-building buildings x-where-to-build y-where-to-build)
  (ormap (lambda (b)
           (match-define (building x y _) b)
           (and (equal? x x-where-to-build) (equal? y y-where-to-build) b))
         buildings))

;; X [Listof X] -> [Listof X]
;; "replace" an x-es on lox with the same location by x
(define (replace x lox)
  (cons x lox))

;; Building -> Building
(define (build-on b)
  (match-define (building x y z) b)
  (building x y (+ z 1)))

;; ---------------------------------------------------------------------------------------------------
;; TEST FRAMEWORK for Boards (needed here to get bindings from top-level modules)

(module* test-support #f
  (provide ->board)

  (require "token.rkt")
  (require rackunit)
  
  #; ([Listof [Listof (U Integer [List Integer Letter])]] -> (U Board  #false))
  (define (->board x)
    (define (+token accu cell x y)
      (if (integer? cell) accu (cons (token (second cell) x y) accu)))
    (define tokens (traverse-literal-board x +token))
    (define (+building accu cell x y)
      (cons (building x y (if (integer? cell) cell (first cell))) accu))
    (define buildings (traverse-literal-board x +building))
    (and (exactly-2-tokens-of-2-kinds tokens) (board tokens buildings)))

  #; ([Listof Token] -> Boolean)
  (define (exactly-2-tokens-of-2-kinds tokens)
    (define names (map token-name tokens))
    (and (or (= (length names) 4) (error '->board "too few tokens"))
         (let* ([fst (first names)]
                [names-first (remove* (list fst) names)])
           (and (or (= (length names-first) 2) (error '->board "too few tokens of kind ~a" fst))
                (let* ([snd  (first names-first)]
                       [names-other (remove* (list snd) names-first)])
                  (or (empty? names-other) (error '->board "too few tokens of kind ~a" snd)))))))
  
  #; (All (Y X) [Listof [Listof Y]] [[Listof X] Y N N -> X] -> [Listof X])
  (define (traverse-literal-board x f)
    (for/fold ([tokens '()]) ([row x][n-s (in-naturals)])
      (for/fold ([tokens tokens]) ([cell row][e-w (in-naturals)])
        (f tokens cell e-w n-s))))

  (check-exn exn:fail?
             (lambda ()
               (define one-missing (list (token "x" 0 0)  (token "x" 0 1) (token "o" 1 1)))
               (exactly-2-tokens-of-2-kinds one-missing)))

  (check-exn exn:fail?
             (lambda ()
               (define xxxo (list (token "x" 0 0)  (token "x" 0 1) (token "x" 0 1) (token "o" 1 1)))
               (exactly-2-tokens-of-2-kinds xxxo)))

  (check-exn exn:fail?
             (lambda ()
               (define ooox `(,(token "x" 0 0) ,(token "x" 0 1) ,(token "o" 0 1) ,(token "y" 1 1)))
               (exactly-2-tokens-of-2-kinds ooox))))

(module+ test
  (require (submod ".." test-support))
  (require (for-syntax (submod ".." test-support)))
  
  (begin-for-syntax
    (define CELL #px"(\\d*)([a-z])")
  
    (define-syntax-class cell
      (pattern x:integer
               #:attr v #'x
               #:attr w #'x)
      (pattern x:id
               #:do [(define sym (syntax-e #'x))
                     (define str (symbol->string sym))
                     (define mat (regexp-match CELL str))]
               #:fail-unless (and mat (string->number (second mat))) "not a cell spec"
               #:attr v #`(list #,(string->number (second mat)) #,(third mat))
               #:attr w #`(#,(string->number (second mat)) #,(third mat))))

    (define-syntax-class cell+
      (pattern x:cell
               #:attr v #'x.v)
      (pattern ((~literal unquote) x)
               #:attr v #'x))

    (define-syntax-class literal-board
      [pattern [[x:cell ...] ...]
               #:do [(define x-board #'((x.w ...) ...))
                     (define d-board (syntax->datum x-board))
                     (define checked
                       (with-handlers ([exn:fail? (lambda (xn)
                                                    (define msg (exn-message xn))
                                                    (define cnm (exn-continuation-marks xn))
                                                    (define stx (list this-syntax))
                                                    (raise (exn:fail:syntax msg cnm stx)))])
                         (->board d-board)))]
               #:fail-unless checked "not a board"
               #:attr lit #'#true
               #:attr board #'(list (list x.v ...) ...)]
      [pattern [[x:cell+ ...] ...]
               #:do [(define x-board #'(list (list x.v ...) ...))]
               #:fail-unless x-board "not a board"
               #:attr lit #'#false
               #:attr board x-board]))
  
  (define-syntax (define-board stx)
    (syntax-parse stx [(_ n:id b:literal-board) #'(define n (->board b.board))]))

  (check-equal? (let ()
                  (define-board b
                    [[2x 2o]
                     [1x 1o 3]])
                  b)
                (board (list (token "x" 0 0) (token "o" 1 0) (token "x" 0 1) (token "o" 1 1))
                       (list (building 0 0 2)
                             (building 1 0 2)
                             (building 0 1 1)
                             (building 1 1 1)
                             (building 2 1 3))))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (let ()
                  (define-board b
                    [[2x 2o 1x]
                     [1x 1o 3]])
                  b))))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (let ()
                  (define-board b
                    [[2x]
                     [1x 1o 3]])
                  b))))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (let ()
                  (define-board b
                    [[2x 1x]
                     [1x 1o 3]])
                  b)))))

;; ---------------------------------------------------------------------------------------------------
;; TESTS
(module+ test
  (require (submod ".."))

  (void (hash (board '() '()) 1)) ;; cover first hash code function 
  
  (define lox0 (list (token "o" 2 1) (token "o" 1 1) (token "x" 2 0) (token "x" 1 0)))
  (check-equal? (apply init lox0) (board lox0 '()))

  (define board0 (apply init lox0))

  (check-true  ((on-board? board0) (first lox0)))
  (check-false ((on-board? board0) (token "o" 3 3)))

  (check-equal? (active-tokens lox0) (apply set lox0))

  (define lox1
    (list (token "o" 2 1) (token "x" 2 1) (token "o" 1 1) (token "x" 2 0) (token "x" 1 0)))
  (check-equal?
   (active-tokens lox1) (set (token "o" 2 1) (token "x" 2 1) (token "o" 1 1) (token "x" 2 0)))

  (define lox2
    (list (token "o" 2 1) (token "x" 2 1) (token "x" 2 0) (token "o" 1 1) (token "x" 1 0)))
  (check-equal?
   (active-tokens lox2) (set (token "o" 2 1) (token "x" 2 1) (token "o" 1 1) (token "x" 2 0))))

(module+ test
  (define board1
    (board
     (list
      (token "x" 0 0)
      (token "o" 2 1)
      (token "o" 1 1)
      (token "x" 2 0)
      (token "x" 1 0))
     (list
      (building 2 1 1)
      (building 1 1 2)
      (building 0 1 3)
      (building 2 0 1)
      (building 1 0 2)
      (building 0 0 3))))

  (define board2
    (board
     (list
      (token "o" 2 1)
      (token "o" 1 1)
      (token "x" 2 0)
      (token "x" 0 0))
     (list
      (building 2 1 1)
      (building 1 1 2)
      (building 0 1 3)
      (building 2 0 1)
      (building 1 0 2)
      (building 0 0 3))))
  
  (check-equal? board1 board2)
  
  (define (board-move ss tt)
    (define-board b1
      [[,ss ,tt 1x]
       [3   2o  1o]])
    b1)

  (define b1-before (board-move 3 (list 2 "x")))
  (define b1-after  (board-move (list 3 "x") 2))
  
  (define expected-b
    (board
     (list (token "o" 2 1) (token "o" 1 1) (token "x" 2 0) (token "x" 1 0))
     (list (building 2 1 1) (building 1 1 2) (building 0 1 3)
           (building 2 0 1) (building 1 0 2) (building 0 0 3))))
  (check-equal? b1-before expected-b)

  (check-false  (find-building (board-buildings b1-before) 0 2))
  (check-equal? (find-building (board-buildings b1-before) 0 1) (building 0 1 3))

  (check-equal? (height-of b1-before 0 2) 0)
  (check-equal? (height-of b1-before 0 1) 3)

  (check-equal? (find-building (board-buildings b1-before) (+ 1 PUT) (+ 0 SOUTH)) (building 1 1 2))
  
  (check-false (location-free-of-token? b1-before (+ 1 PUT) (+ 0 SOUTH)))
  (check-true  (location-free-of-token? b1-before (+ 1 WEST) (+ 0 SOUTH)))

  (check-equal? (move b1-before (token "x" 1 0) WEST PUT) b1-after)

  (define (board-build ss tt (ff 0))
    (define-board b1
      [[,ss ,tt ,ff]
       [2x   2o  1o]])
    b1)
  
  (check-equal? (build (board-build 2 (list 2 "x")) (token "x" 1 0) WEST PUT)
                (board-build 3 (list 2 "x")))

  (define-board b3-before [[2x 2o]   [2x   2o  1]])
  (define-board b3-after  [[2x 2o 1] [2x   2o  1]])
  (check-equal? (build b3-before (token "o" 1 0) EAST PUT) b3-after))
                
