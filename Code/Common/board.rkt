#lang racket

;; The Board

;; what knowledge is turned into information here and represented with data:
;; -- where the workers are
;; -- where buildings are
;; -- how tall buildings are
;; -- whether the board is in a "final state"
;; ---------------------------------------------------------------------------------------------------
(require "../Lib/require.rkt")
(require+ "worker.rkt" worker? in-range? east-west/c north-south/c at-distinct-places)

(provide

 ;; type initial-placement
 in-range? 

 ;; type directions
 east-west/c north-south/c EAST WEST NORTH SOUTH PUT

 ;; type worker? and operations on worker?s 
 worker worker? all-directions-to-neighbors move-worker stay-on-board? 
 
 ;; type Building = (building Range Range N)
 MAX-HEIGHT ; a buidling is called 'capped' if its MAX-HEIGHT stories tall. 
 TOP-FLOOR  ; this is the victory story 

 ;; type board
 board?

 ;; Worker? Worker? Worker? Worker? -> Board
 ; board

 (contract-out 
  (init
   ;; create the board and place the four worker?s on it
   (->i ((t1 worker?) (t2 worker?) (t3 worker?) (t4 worker?))
        #:pre (t1 t2 t3 t4) (at-distinct-places (list t1 t2 t3 t4))
        (r board?)))

  (named-workers
   ;; retrieve the workers of the given name on this board
   (-> board? string? (list/c worker? worker?)))

  (on?
   ;; is this the name of a player on this boar? 
   (-> board? (-> string? boolean?)))
  
  (on-board?
   ;; does this worker exist on this board?
   (-> board? (-> worker? boolean?)))
  
  (height-of
   (->* (board? worker?) (east-west/c north-south/c) natural-number/c))
 
  (location-free-of-worker?
   ;; there is no worker on (x,y) relative to given worker on this board 
   (-> board? worker? east-west/c north-south/c boolean?))

  (is-move-a-winner?
   ;; did the move of the worker end the game on this board?
   (->i ((b board?) (t (b) (and/c worker? (on-board? b))) (e-w east-west/c) (n-s north-south/c))
        #:pre/name (t e-w n-s) "remains on board"
        (let*-values ([(new-t) (move-worker t e-w n-s)]
                      [(x y) (worker-location new-t)])
          (and (in-range? x) (in-range? y)))
        (r boolean?)))
  
  (move
   ;; move the worker one step in the given direction
   ;; (move will be called from admin only; no checks needed to ensure legality of move)
   (->i ((b board?) (t (b) (and/c worker? (on-board? b))) (e-w east-west/c) (n-s north-south/c))
        (r board?)))
 
  (build
   ;; add a level to the buidling that is in the specified direction
   ;; (move will be called from admin only; no checks needed to ensure legality of build
   (->i ((b board?) (t (b) (and/c worker? (on-board? b))) (e-w east-west/c) (n-s north-south/c))
        (r board?)))))

(module+ test
  (provide
   ;; SYNTAX
   #; (define-board name [[x-y ...] ...])
   ;; defines a literal board with cells x-y ...
   ;; Each cell must either be an integer or an identifier that combines a natural with a letter.
   ;; An integer denotes a building of that height.
   ;; An identifier denotes a building of that height with a worker named by the letter atop.
   ;; The grid's origin is the top left. Moving down is moving south, moving right means moving east.
   #;[[0 0 1x]
      [2y]
      [1x 3 2y]]
   ;; is a board with two "x" workers at (2,0) and (0,2), each at height 1,
   ;; and two "y" workers at (0,1) and (2,2), each at height 2; 
   ;; there is one other buildig at (1,2) of height 3. 
   define-board))

;; ---------------------------------------------------------------------------------------------------
;; DEPENDENCIES

(require- "worker.rkt" worker? in-range? east-west/c north-south/c at-distinct-places)
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

(struct board (workers buildings)
  #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc b1 b2 equal?)
     (match-define (board b1t b1b) b1)
     (match-define (board b2t b2b) b2)
     (and (set=? (apply set b1t) (apply set b2t))
          (set=? (set-from b1b same-building) (set-from b2b same-building))))
   ;; the following two are, well, inappropriate in general 
   (define (hash-proc bd rhash) 0)
   (define (hash2-proc bd rhash2) 1)])

(define (init worker1 worker2 worker3 worker4)
  (board (list worker1 worker2 worker3 worker4) '()))

(define (named-workers b n)
  (with board b (filter (lambda (t) (string=? (worker-name t) n)) workers)))

(define ((on? b) n)
  (with board b (cons? (member n (map worker-name workers)))))

(define ((on-board? b) t)
  (with board b (cons? (member t workers))))

(define (is-move-a-winner? b t e-w n-s)
  (= (height-of b t e-w n-s) TOP-FLOOR))

(define (height-of b t (e-w PUT) (n-s PUT))
  (with board b
        (define-values (xt yt) (worker-location t))
        (define-values (x  y)  (values (+ xt e-w) (+ yt n-s)))
        (define is-there-a-building (find-building buildings x y))
        (if (boolean? is-there-a-building) 0 (building-height is-there-a-building))))

(define (location-free-of-worker? b t e-w n-s)
  (define-values (xt yt) (worker-location t))
  (define-values (x0 y0) (values (+ xt e-w) (+ yt n-s)))
  (for/and ((t (board-workers b)))
    (define-values (x y) (worker-location t))
    (not (and (= x0 x) (= y0 y)))))

(define (move b worker e-w n-s)
  (with board b (board (replace (move-worker worker e-w n-s) worker workers) buildings)))

(define (build b worker e-w n-s)
  (with board b
        (define-values (x-where-to-build y-where-to-build) (neighbor-location worker e-w n-s))
        (define is-there-a-building (find-building buildings x-where-to-build y-where-to-build))
        (define the-building (or is-there-a-building (building x-where-to-build y-where-to-build 0)))
        (board workers (replace (build-on the-building) the-building buildings))))

;; [Listof Building] Range Range -> (U Building #false)
(define (find-building buildings x-where-to-build y-where-to-build)
  (ormap (lambda (b)
           (match-define (building x y _) b)
           (and (equal? x x-where-to-build) (equal? y y-where-to-build) b))
         buildings))

;; X X [Listof X] -> [Listof X]
;; "replace" an x-es on lox with the same location by x
(define (replace x old lox)
  (cons x (remove old lox)))

;; Building -> Building
(define (build-on b)
  (match-define (building x y z) b)
  (building x y (+ z 1)))

;; ---------------------------------------------------------------------------------------------------
;; TEST FRAMEWORK for Boards (needed here to get bindings from top-level modules)

(module* test-support #f
  (provide ->board CELL)

  (require "worker.rkt")
  (require rackunit)

  (define CELL #px"(\\d)([a-z])")
  
  #; ([Listof [Listof (U Integer [List Integer Letter])]] -> (U Board  #false))
  (define (->board x)
    (define (+worker accu cell x y)
      (cond
        [(integer? cell) accu]
        [(pair? cell) (cons (worker (second cell) x y) accu)]
        [(and (symbol? cell) (regexp-match CELL (symbol->string cell)))
         => (match-lambda [`(,_all ,height ,name) (cons (worker name x y) accu)])]
        [else (error '->board "bad Racket value for cell: ~e" cell)]))
    (define workers (traverse-literal-board x +worker))
    (define (+building accu cell x y)
      (define b
        (building
         x y
         (cond
           [(integer? cell) cell]
           [(pair? cell) (first cell)]
           [(and (symbol? cell) (regexp-match CELL (symbol->string cell)))
            => (match-lambda [`(,_all ,height ,name) (string->number height)])])))
      (cons b accu))
    (define buildings (traverse-literal-board x +building))
    (and (exactly-2-workers-of-2-kinds workers) (board workers buildings)))

  #; ([Listof Worker?] -> Boolean)
  (define (exactly-2-workers-of-2-kinds workers)
    (define names (map worker-name workers))
    (and (or (= (length names) 4) (error '->board "too few workers"))
         (let* ([fst (first names)]
                [names-first (remove* (list fst) names)])
           (and (or (= (length names-first) 2) (error '->board "too few workers of kind ~a" fst))
                (let* ([snd  (first names-first)]
                       [names-other (remove* (list snd) names-first)])
                  (or (empty? names-other) (error '->board "too few workers of kind ~a" snd)))))))
  
  #; (All (Y X) [Listof [Listof Y]] [[Listof X] Y N N -> X] -> [Listof X])
  (define (traverse-literal-board x f)
    (for/fold ([workers '()]) ([row x][n-s (in-naturals)])
      (for/fold ([workers workers]) ([cell row][e-w (in-naturals)])
        (f workers cell e-w n-s))))

  (check-exn exn:fail?
             (lambda ()
               (define one-missing (list (worker "x" 0 0)  (worker "x" 0 1) (worker "o" 1 1)))
               (exactly-2-workers-of-2-kinds one-missing)))

  (check-exn exn:fail?
             (lambda ()
               (define ws (list (worker "x" 0 0)  (worker "x" 0 1) (worker "x" 0 1) (worker "o" 1 1)))
               (exactly-2-workers-of-2-kinds ws)))

  (check-exn exn:fail?
             (lambda ()
               (define ws `(,(worker "x" 0 0) ,(worker "x" 0 1) ,(worker "o" 0 1) ,(worker "y" 1 1)))
               (exactly-2-workers-of-2-kinds ws))))

(module+ test
  (require (submod ".." test-support))
  (require (for-syntax (submod ".." test-support)))
  
  (begin-for-syntax
    
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

  (define expected-board
    (board (list (worker "x" 0 0) (worker "o" 1 0) (worker "x" 0 1) (worker "o" 1 1))
           (list (building 0 0 2)
                 (building 1 0 2)
                 (building 0 1 1)
                 (building 1 1 1)
                 (building 2 1 3)))))

(module+ test ;; checking define-board at run-time and syntax-time 
  (check-equal? (let () (define-board b [[2x 2o] [1x 1o 3]]) b) expected-board)
  (check-equal? (let () (define-board b [[2x 2o] [1x ,'1o 3]]) b) expected-board)

  (check-exn exn:fail? (lambda () (define-board b [[2x ,'o1][2o 1x]]) b))
  (check-exn exn:fail? (lambda () (define-board b [[2x ,'o1][2o 1x]]) b))
  (check-exn exn:fail? (lambda () (define-board b [[2x ,"o1"][2o 1x]]) b))

  (check-exn
   exn:fail:syntax?
   (lambda () (convert-compile-time-error (let () (define-board b [[2x 2o 1x] [1x 1o 3]]) b))))

  (check-exn
   exn:fail:syntax?
   (lambda () (convert-compile-time-error (let () (define-board b [[2x] [1x 1o 3]]) b))))

  (check-exn
   exn:fail:syntax?
   (lambda () (convert-compile-time-error (let () (define-board b [[2x 1x] [1x 1o 3]]) b)))))

;; ---------------------------------------------------------------------------------------------------
;; TESTS
(module+ test ;; basic functions 
  (require (submod ".."))

  (void (hash (board '() '()) 1)) ;; cover first hash code function 
  
  (define lox0 (list (worker "o" 2 1) (worker "o" 1 1) (worker "x" 2 0) (worker "x" 1 0)))
  (check-equal? (apply init lox0) (board lox0 '()))
  
  (define board0 (apply init lox0))

  (check-true  ((on-board? board0) (first lox0)))
  (check-false ((on-board? board0) (worker "o" 3 3)))

  (check-true  ((on? board0) "o"))
  (check-false ((on? board0) "xxx"))

  (check-equal? (named-workers board0 "x") (list (worker "x" 2 0) (worker "x" 1 0)))

  (define-board board1
    [[3x 2  1x]
     [3  2o 1o]])

  (define board2
    (board
     (list
      (worker "o" 2 1)
      (worker "o" 1 1)
      (worker "x" 2 0)
      (worker "x" 0 0))
     (list
      (building 2 1 1)
      (building 1 1 2)
      (building 0 1 3)
      (building 2 0 1)
      (building 1 0 2)
      (building 0 0 3))))
  
  (check-equal? board1 board2))

(module+ test ;; complex functions, including define-board 
  (define (board-move ss tt)
    (define-board b1
      [[,ss ,tt 1x]
       [3   2o  1o]])
    b1)
  
  (define b1-before (board-move 3 '2x))
  (define b1-after  (board-move '3x 2))

  (check-false (is-move-a-winner? b1-before (worker "x" 1 0) EAST PUT))
  (check-true  (is-move-a-winner? b1-after (worker "x" 0 0) PUT SOUTH))
  
  (check-false  (find-building (board-buildings b1-before) 0 2))
  (check-equal? (find-building (board-buildings b1-before) 0 1) (building 0 1 3))
  (check-equal? (find-building (board-buildings b1-before) (+ 1 PUT) (+ 0 SOUTH)) (building 1 1 2))

  (check-equal? (height-of b1-before (worker "x" 0 2)) 0)
  (check-equal? (height-of b1-before (worker "x" 0 1)) 3)
  
  (check-false  (location-free-of-worker? b1-before (worker "x" 1 0) PUT SOUTH))
  (check-true   (location-free-of-worker? b1-before (worker "x" 1 0) WEST SOUTH))

  (check-equal? (move b1-before (worker "x" 1 0) WEST PUT) b1-after)

  (define (board-build ss tt (ff 0))
    (define-board b1
      [[,ss ,tt ,ff]
       [2x   2o  1o]])
    b1)
  
  (check-equal? (build (board-build 2 '2x) (worker "x" 1 0) WEST PUT) (board-build 3 '2x))

  (define-board b3-before [[2x 2o]   [2x   2o  1]])
  (define-board b3-after  [[2x 2o 1] [2x   2o  1]])
  (check-equal? (build b3-before (worker "o" 1 0) EAST PUT) b3-after))
                
