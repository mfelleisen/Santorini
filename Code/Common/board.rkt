#lang racket

;; The Board

;; what knowledge is turned into information here and represented with data:
;; -- where the workers are
;; -- where buildings are
;; -- how tall buildings are
;; -- whether the board is in a "final state"

;; ---------------------------------------------------------------------------------------------------
(define DIM 5)
(define in-range? (integer-in 0 DIM))
(define init/c (list/c worker? in-range? in-range?))

(provide
 DIM
 ;; type Range = [0,DIM)
 in-range?

 ;; type board
 board?

 board ;; for debugging 
 
 (contract-out 
  (init
   ;; create the board and place the four worker?s on it
   (->i ((t1 init/c) (t2 init/c) (t3 init/c) (t4 init/c))
        #:pre/name (t1 t2 t3 t4) "must be at distinct places"
        (let ([L (map rest (list t1 t2 t3 t4))]) (= (set-count (apply set L)) (length L)))
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

  (all-directions-to-neighbors
   ;; compute all possible directions to a neighboring field from this worker
   ;; GUARANTEE (0,0) is not a part of the directions 
   (-> board? worker? (listof (list/c east-west/c north-south/c))))

  (stay-on-board?
   ;; does this worker stay in range if it moves in the specified direction?
   ;; ASSUME worker is in range 
   (-> board? worker? east-west/c north-south/c boolean?))
  
  (height-of
   ;; the height of the building where the worker is located or looking at on this board 
   (->* (board? worker?) (east-west/c north-south/c) natural-number/c))
 
  (location-free-of-worker?
   ;; there is no worker on (x,y) relative to given worker on this board 
   (-> board? worker? east-west/c north-south/c boolean?))

  (move
   ;; move the worker one step in the given direction
   ;; (move will be called from admin only; no checks needed to ensure legality of move)
   (->i ((b board?) (t (b) (and/c worker? (on-board? b))) (e-w east-west/c) (n-s north-south/c))
        #:pre/name (b t e-w n-s) "remains on board" (stay-on-board? b t e-w n-s)
        (r board?)))
  
  (build
   ;; add a level to the buidling that is in the specified direction
   ;; (move will be called from admin only; no checks needed to ensure legality of build
   (->i ((b board?) (t (b) (and/c worker? (on-board? b))) (e-w east-west/c) (n-s north-south/c))
        #:pre/name (b t e-w n-s) "builds on board" (stay-on-board? b t e-w n-s)
        (r board?)))
 
  (is-move-a-winner?
   ;; did the move of the worker end the game on this board?
   (->i ((b board?) (t (b) (and/c worker? (on-board? b))) (e-w east-west/c) (n-s north-south/c))
        #:pre/name (b t e-w n-s) "remains on board" (stay-on-board? b t e-w n-s)
        (r boolean?)))))

(module+ test
  (provide
   ;; SYNTAX
   #; (define-board name [[x-y ...] ...])
   ;; defines a literal board with cells x-y ...
   ;; Each cell must be
   ;; -- an N, which denotes a building of this height or
   ;; -- an identifier whose name
   ;;    -- starts with an N, which denotes the height of the building 
   ;;    -- is followed by [a-z]+ and
   ;;    -- ends in 1 or 2.
   ;;    The rest of the identifier denotes a worker.
   ;; The grid's origin is the top left. Moving down is moving south, moving right means moving east.
   ;; 
   ;; EXAMPLE:
   ;; --------
   #;[[0 0 1x1]
      [2y1]
      [1x2 3 2y2]]
   ;; is a board with two "x" workers at (2,0) and (0,2), each at height 1,
   ;; and two "y" workers at (0,1) and (2,2), each at height 2; 
   ;; there is one other buildig at (1,2) of height 3. 
   define-board))

;; ---------------------------------------------------------------------------------------------------
;; DEPENDENCIES

(require "directions.rkt")
(require "buildings.rkt")
(require "worker.rkt")
(require "../Lib/set-from.rkt")

(require (for-syntax syntax/parse))
(module+ test
  (require rackunit)
  (require (for-syntax racket/list))
  (require syntax/macro-testing))

;; ---------------------------------------------------------------------------------------------------
;; The BOARD

;; Board     = (board Worker* Building*)
;; Worker*   = [List Worker N N]
;; Building* = [List Buiolding N N]

(define to-x second)
(define to-y third)

(struct board (workers buildings)
  #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc b1 b2 e) (board-equal? b1 b2 e))
   ;; the following two are, well, inappropriate in general 
   (define (hash-proc bd rhash) 0)
   (define (hash2-proc bd rhash2) 1)]
  #:methods gen:custom-write
  [;; Board OutputPort Boolean? -> Void 
   (define (write-proc b op mode)
     (define buildings (board-buildings b))
     
     (define workers   (board-workers b))
     (define widest    (argmax string-length (map (compose worker-name first) workers)))
     (define blank     (make-string (+ (string-length widest) 2) #\space))
     
     (define both  (append workers buildings))
     (define x-max (maximum to-x both))
     (define y-max (maximum to-y both))
     
     (define reversed-lines ;; walk the grid [0,x-max] x [0,y-max]
       (for/fold ([lines '()]) ((y (in-range (+ y-max 1))))
         (define cleansed
           (remove-trailing-0s
            (for/list ((x (in-range (+ x-max 1))))
              (define bldg (find-building buildings x y))
              (define hght (if (boolean? bldg) "0" (number->string (building-height bldg))))
              (define wrkr (find-worker workers x y))
              (define name (if (boolean? wrkr) "  " (worker-name+no wrkr)))
              (string-append hght name))))
         (cons (format " [~a]" (string-join cleansed)) lines)))

     (parameterize ([current-output-port op])
       (let print ([lines (rest reversed-lines)])
         (cond
           [(empty? (rest lines))
            (displayln (string-append "[" (substring (first lines) 1)))]
           [else
            (print (rest lines))
            (displayln (first lines))]))
       (displayln (string-append (first reversed-lines) "]"))))

   ;; [X -> N] [Listof X] -> N
   (define (maximum xselector lox)
     (xselector (argmax xselector lox)))
   

   ;; {Listof String] -> [Listof String]
   (define (remove-trailing-0s los)
     (cond
       [(empty? los) '()]
       [else (define fst (first los))
             (define rst (remove-trailing-0s (rest los)))
             (if (and (empty? rst) (string=? fst "0  ")) '() (cons fst rst))]))])

(define (init worker1 worker2 worker3 worker4)
  (board (list worker1 worker2 worker3 worker4) '()))

(define (named-workers b n)
  (for*/list ((t (board-workers b)) (w (in-value (first t))) #:when (string=? (worker-name w) n))
    w))

(define ((on? b) n)
  (cons? (member n (map (compose worker-name first) (board-workers b)))))

(define ((on-board? b) t)
  (cons? (member t (map first (board-workers b)))))

(define (all-directions-to-neighbors b t)
  (define-values (x y) (worker-location b t))
  (for*/list ((e-w `(,WEST ,PUT ,EAST))
              (n-s `(,NORTH ,PUT ,SOUTH))
              (new-e-w (in-value (+ x e-w)))
              (new-n-s (in-value (+ y n-s)))
              #:when (and (not (= 0 e-w n-s)) (in-range? new-e-w) (in-range? new-n-s)))
    (list e-w n-s)))

(define (stay-on-board? board t e-w n-s)
  (define-values (x y) (worker-location board t))
  (and (in-range? (+ x e-w)) (in-range? (+ y n-s))))

(define (is-move-a-winner? b t e-w n-s)
  (= (height-of b t e-w n-s) TOP-FLOOR))

(define (location-free-of-worker? b t e-w n-s)
  (define-values (x0 y0) (shift b t e-w n-s))
  (andmap (match-lambda [`(,_w ,x ,y) (not (and (= x0 x) (= y0 y)))]) (board-workers b)))

(define (height-of b t (e-w PUT) (n-s PUT))
  (define-values (x y) (shift b t e-w n-s))
  (define is-there-a-building (find-building (board-buildings b) x y))
  (if (boolean? is-there-a-building) 0 (building-height is-there-a-building)))

(define (move b worker e-w n-s)
  (define-values (x-where-to-move y-where-to-move) (shift b worker e-w n-s))
  (board (move-worker (board-workers b) worker x-where-to-move y-where-to-move) (board-buildings b)))

(define (build b worker e-w n-s)
  (define-values (x-where-to-build y-where-to-build) (shift b worker e-w n-s))
  (board (board-workers b) (build-on (board-buildings b) x-where-to-build y-where-to-build)))

;; ---------------------------------------------------------------------------------------------------
;; auxiliaries

;; ([Listof (List Worker N N)] Worker N N -> (Listof Worker))
(define (move-worker workers w0 x-where-to-move y-where-to-move)
  (define nu (list w0 x-where-to-move y-where-to-move))
  (replace nu (lambda (w) (equal? (first w) w0)) workers))

;; ((Listof Building) N N -> (Listof Building))
(define (build-on buildings x y)
  (define exists-one (find-building buildings x y))
  (match exists-one
    [(? boolean?) (cons (list (building 1) x y) buildings)]
    [(building z)
     (replace (list (building (+ z 1)) x y) (curry same-building (list exists-one x y)) buildings)]))

;; Board Worker EAST-WEST NORTH-SOUTH -> (values N N)
(define (shift b worker e-w n-s)
  (define-values (x y) (worker-location b worker))
  (values (+ x e-w) (+ y n-s)))

;; Board Worker -> (values in-range? in-range?)
(define (worker-location b w)
  (match-define `(,_w ,x ,y)
    (for*/first ([t (board-workers b)] [ww (in-value (first t))] #:when (equal? w ww)) t))
  (values x y))

;; [Listof Building] Range Range -> (U N #false)
(define (find-building buildings x0 y0)
  (finder buildings (match-lambda [`(,b ,x ,y) (and (= x x0) (= y y0) b)])))

;; [Listof Worker] Range Range -> (U String #false)
(define (find-worker workers x0 y0)
  (finder workers (match-lambda [`(,w ,x ,y) (and (= x x0) (= y y0) w)])))

;; ([Listof X] [X -> Boolean] -> (U X #false))
(define (finder lox matcher)
  (ormap matcher lox))

;; (X (X -> Boolean) [Listof X] -> [Listof X])
(define (replace nux cmp lox)
  (cons nux (remf cmp lox)))

;; Board Board -> Boolean 
(define (board-equal? b1 b2 equal?)
  (match-define (board 1workers 1buildings) b1)
  (match-define (board 2workers 2buildings) b2)
  (and (set=? (apply set 1workers) (apply set 2workers))
       (set=? (buildings->set 1buildings) (buildings->set 2buildings))))

;; [Listof Building] -> [Setof Building]
(define (buildings->set b:list)
  (define b-0-buildings (filter (lambda (b) (> (building-height (first b)) 0)) b:list))
  (apply set b-0-buildings))

;; Building -> [Building -> Building]
(define (same-building b1)
  (match-define (list (building z1) x1 y1) b1)
  (lambda (b2)
    (match-define (list (building z2) x2 y2) b2)
    (and (= x1 x2) (= y1 y2))))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; board equality 
  (define board-eq1
    (board 
     (list
      (list (worker "cd2") 4 4)
      (list (worker "cd1") 2 2)
      (list (worker "mf2") 2 1)
      (list (worker "mf1") 0 0))
     (list
      (list (building 1) 5 5)
      (list (building 0) 3 3)
      (list (building 0) 2 3)
      (list (building 0) 1 3)
      (list (building 0) 0 3)
      (list (building 1) 3 2)
      (list (building 0) 2 2)
      (list (building 0) 1 2)
      (list (building 0) 0 2)
      (list (building 0) 2 1)
      (list (building 0) 1 1)
      (list (building 0) 0 1)
      (list (building 0) 0 0))))

  (define board-eq2
    (board 
     (list
      (list (worker "cd2") 4 4)
      (list (worker "cd1") 2 2)
      (list (worker "mf2") 2 1)
      (list (worker "mf1") 0 0))
     (list
      (list (building 1) 5 5)
      (list (building 0) 4 5)
      (list (building 0) 3 5)
      (list (building 0) 2 5)
      (list (building 0) 1 5)
      (list (building 0) 0 5)
      (list (building 0) 4 4)
      (list (building 0) 3 4)
      (list (building 0) 2 4)
      (list (building 0) 1 4)
      (list (building 0) 0 4)
      (list (building 0) 3 3)
      (list (building 0) 2 3)
      (list (building 0) 1 3)
      (list (building 0) 0 3)
      (list (building 1) 3 2)
      (list (building 0) 2 2)
      (list (building 0) 1 2)
      (list (building 0) 0 2)
      (list (building 0) 2 1)
      (list (building 0) 1 1)
      (list (building 0) 0 1)
      (list (building 0) 0 0))))

  (check-true (equal? board-eq1 board-eq2)))



;; ---------------------------------------------------------------------------------------------------
;; TEST FRAMEWORK for Boards (needed here to get bindings from top-level modules)
(module* test-support #f
  (provide ->board CELL)

  (require "worker.rkt")
  (require rackunit)

  (define CELL #px"(\\d)([a-z]*[1,2])")
  
  #; ([Listof [Listof (U Integer [List Integer Letter])]] -> (U Board  #false))
  (define (->board x)
    (define (+worker accu cell x y)
      (cond
        [(integer? cell) accu]
        [(pair? cell) (cons (list (second cell) (worker (second cell)) x y) accu)]
        [(and (symbol? cell) (regexp-match CELL (symbol->string cell)))
         => (match-lambda [`(,_all ,_height ,name) (cons (list name (worker name) x y) accu)])]
        [else (error '->board "bad Racket value for cell: ~e" cell)]))
    (define full-name+workers+loc (traverse-literal-board x +worker))
    (define (+building accu cell x y)
      (define b
        (list 
         (building
          (cond
            [(integer? cell) cell]
            [(pair? cell) (first cell)]
            [(and (symbol? cell) (regexp-match CELL (symbol->string cell)))
             => (match-lambda [`(,_all ,height ,name) (string->number height)])]))
         x y))
      (cons b accu))
    (define buildings (traverse-literal-board x +building))
    (and (exactly-2-workers-of-2-kinds full-name+workers+loc)
         (board (map rest full-name+workers+loc) buildings)))

  #; ([Listof Worker?] -> Boolean)
  (define (exactly-2-workers-of-2-kinds full-name+workers+loc)
    (define names (for/list ((w full-name+workers+loc)) (list (first w) (worker-name (second w)))))
    (unless (= (length names) 4) (error '->board "too few workers"))
    
    (match-define `(,full-fst ,fst) (first names))
    (define names-fst (remf* (lambda (n) (equal? (second n) fst)) names))
    (unless (= (length names-fst) 2) (error '->board "wrong number of ~a workers" fst))
    (define fulls1 (map first (filter (lambda (n) (equal? (second n) fst)) names)))
    
    (match-define `(,full-snd ,snd) (first names-fst))
    [define names-snd (remf* (lambda (x) (equal? (second x) snd)) names-fst)]
    (define fulls2 (map first (filter (lambda (n) (equal? (second n) snd)) names)))
    
    (unless(empty? names-snd) (error '->board "a third kind of worker: ~a" names-snd))

    (when (equal? (first fulls1) (second fulls1)) (error '->board "duplicate ~a worker" fst))
    (when (equal? (first fulls2) (second fulls2)) (error '->board "duplicate ~a worker" snd))
    #t)
  
  #; (All (Y X) [Listof [Listof Y]] [[Listof X] Y N N -> X] -> [Listof X])
  (define (traverse-literal-board x f)
    (for/fold ([accu '()]) ([row x][n-s (in-naturals)])
      (for/fold ([accu accu]) ([cell row][e-w (in-naturals)])
        (f accu cell e-w n-s))))

  (define-syntax-rule (check-2 re w) (check-exn re (lambda () (exactly-2-workers-of-2-kinds w))))
  
  (check-2 #px"too few workers"
           `(("x1" ,(worker "x1") 0 0) ("x2" ,(worker "x2") 0 1) ("o1" ,(worker "o1") 1 1)))
  (check-2
   #px"wrong number of x workers"
   `(("x1" ,(worker "x1") 0 0) ("x2" ,(worker "x2") 0 1) ("x2" ,(worker "x2") 0 1)
                               ("o1" ,(worker "o1") 1 1)))
  (check-2
   #px"a third kind of worker"
   `(("x1" ,(worker "x1") 0 0) ("x2" ,(worker "x2") 0 1) ("o1" ,(worker "o1") 0 1)
                               ("y1" ,(worker "y1") 1 1))))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; define-board for creating scenarios in a concise form 
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
    (syntax-parse stx [(_ n:id b:literal-board) #'(define n (->board b.board))])))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; checking define-board at run-time and syntax-time
  
  (define expected-board
    (board
     `((,(worker "x1") 0 0) (,(worker "o1") 1 0) (,(worker "x2") 0 1) (,(worker "o2") 1 1))
     `((,(building 2) 0 0) (,(building 2) 1 0) (,(building 1) 0 1) (,(building 1) 1 1)
                           (,(building 3)  2 1))))
  
  (check-equal? (let () (define-board b [[2x1 2o1] [1x2 1o2   3]]) b) expected-board)
  (check-equal? (let () (define-board b [[2x1 2o1] [1x2 ,'1o2 3]]) b) expected-board)

  (check-exn exn:fail? (lambda () (define-board b [[2x1 ,'o1][2o2 1x2]]) b))
  (check-exn exn:fail? (lambda () (define-board b [[2x1 ,'o1][2o2 1x2]]) b))
  (check-exn exn:fail? (lambda () (define-board b [[2x1 ,"o1"][2o2 1x2]]) b))

  (define-syntax-rule
    (check-syn re b)
    (check-exn re (lambda () (convert-compile-time-error (let () (define-board c b) 0)))))

  (check-syn #px"not a cell spec" [[2x 2o 1x] [1x 1o 3]])
  (check-syn #px"too few workers" [[2x1] [1x2 1o1 3]])
  (check-syn #px"wrong number of o workers" [[2x1 1x2] [1x2 1o1 3]])
  
  (check-syn #px"duplicate aa worker" [[2zz1 2zz1] [1aa1 1aa1]])
  (check-syn #px"duplicate zz worker" [[1aa1 1aa1] [2zz1 2zz1]])

  (define-board board1
    [[3x1 2  1x2]
     [3  2o1 1o2]])

  (define board2
    (board
     (list
      (list (worker "o2") 2 1)
      (list (worker "o1") 1 1)
      (list (worker "x2") 2 0)
      (list (worker "x1") 0 0))
     (list
      (list (building 1) 2 1)
      (list (building 2) 1 1)
      (list (building 3) 0 1)
      (list (building 1) 2 0)
      (list (building 2) 1 0)
      (list (building 3) 0 0))))
  
  (check-equal? board1 board2))
;; ---------------------------------------------------------------------------------------------------
(module+ test ;; auxiliary functions 

  (define-board board-find
    [[0x1 0x2]
     [3y1 2y2]])

  (define buildings-find (board-buildings board-find))

  (check-false  (find-building buildings-find 0 2))
  (check-equal? (find-building buildings-find 0 1) (building 3))
  (check-equal? (find-building buildings-find (+ 1 PUT) (+ 0 SOUTH)) (building 2)))

(module+ test ;; external functions
  (require (submod ".."))

  (void (hash (board '() '()) 1)) ;; cover first hash code function 
  
  (define lox0 `((,(worker "o1") 2 1) (,(worker "o2") 1 1) (,(worker "x1") 2 0) (,(worker "x2" )1 0)))
  (check-equal? (apply init lox0) (board lox0 '()))
  
  (define board0 (apply init lox0))

  (check-true  ((on-board? board0) (first (first lox0))))
  (check-false ((on-board? board0) (worker "w2")))

  (check-true  ((on? board0) "o"))
  (check-false ((on? board0) "xxx"))

  (check-equal? (named-workers board0 "x") (list (worker "x1") (worker "x2")))
  
  (define (board-move ss tt)
    (define-board b1
      [[,ss ,tt 1x1]
       [3   2o1 1o2]])
    b1)
  
  (define b1-before (board-move 3 '2x2))
  (define b1-after  (board-move '3x2 2))

  (check-equal? (apply set (all-directions-to-neighbors b1-after (worker "x2")))
                (set (list EAST PUT) (list EAST SOUTH) (list PUT SOUTH)))
  (check-equal? (apply set (all-directions-to-neighbors b1-before (worker "x2")))
                (apply set `((,EAST ,PUT) (,EAST ,SOUTH) (,PUT ,SOUTH) (,WEST ,PUT) (,WEST ,SOUTH))))

  (check-false (stay-on-board? b1-before (worker "x2") PUT NORTH))
  (check-true  (stay-on-board? b1-before (worker "x2") PUT SOUTH))

  (check-false (is-move-a-winner? b1-before (worker "x2") EAST PUT))
  (check-true  (is-move-a-winner? b1-after (worker "x2") PUT SOUTH))
  
  (check-equal? (height-of b1-before (worker "o1") PUT SOUTH) 0)
  (check-equal? (height-of b1-before (worker "x2")) 2)
  
  (check-false  (location-free-of-worker? b1-before (worker "x2") PUT SOUTH))
  (check-true   (location-free-of-worker? b1-before (worker "x2") WEST SOUTH))

  (check-equal? (move b1-before (worker "x2") WEST PUT) b1-after)
  
  (define (board-build ss tt (ff 0))
    (define-board b1
      [[,ss ,tt ,ff]
       [2x1 2o1 1o2]])
    b1)
  
  (check-equal? (build (board-build 2 '2x2) (worker "x2") WEST PUT) (board-build 3 '2x2))

  (define-board b3-before [[2x1 2o1]   [2x2   2o2  1]])
  (define-board b3-after  [[2x1 2o1 1] [2x2   2o2  1]])
  (check-equal? (build b3-before (worker "o1") EAST PUT) b3-after))

(module+ test ;; testing printing

  (define-board print-board
    [[3x1 0y2]
     [0x2 0y1]
     [1   ]])

  (define print-board2 
    (board `((,(worker "x1") 0 0)
             (,(worker "x2") 0 1)
             (,(worker "y2") 1 0)
             (,(worker "y1") 1 1))
           (list (list (building 3) 0 0) (list (building 1) 0 2))))

  (define print-expected "[[3x1 0y2]\n [0x2 0y1]\n [1  ]]\n")

  (check-equal? (with-output-to-string (lambda () (display print-board))) print-expected)  
  (check-equal? (with-output-to-string (lambda () (display print-board2))) print-expected))