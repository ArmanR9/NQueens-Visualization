;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nqueens-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
(require 2htdp/image)

;; The n queens problem consists of finding a way to place n chess queens
;; on a n by n chess board while making sure that none of the queens attack each
;; other. 
;;
;; The BOARD consists of n^2 individual SQUARES arranged in 4 rows of 4 columns.
;; The colour of the squares does not matter. Each square can either be empty
;; or can contain a queen.

;; ==========
;; Data Definitions:

;(@htdd Position)
;(define-struct pos (x y))
;;; Position is (make-pos Natural Natural)
;;; interp. a 0th-based cartesian position system of chess board where
;;;         - x is the 0th-based row of the board
;;;         - y is the 0th based column of the board
;;; CONSTRAINT: x and y must be in [0, (sub1 (sqrt (length m))]

(@htdd Board)
;; Board is one of
;; - empty
;; - (cons Boolean Board)
;; inerp. a flat-list representation of a chess board that has w & l n big, &
;;         n*n area
;;        - true means a queen exists in cell
;;        - false means an empty cell
;; CONSTRAINT: board must have (sqrt (length bd)) be a Natural (perfect square)

(@htdd Position)
;; Position is one of
;; - 0
;; - (add1 Position)
;; interp. 0-based position inside of chess board where
;;        - row is (quotient p (sqrt (length bd)))
;;        - column is (remainder p (sqrt (length bd)))
;; CONSTRAINT: Position must be in [0, (sub1 (length bd))]
 
;; ==========
;; Constants:

(define O #f) ; Empty spot on board
(define Q #t) ; Queen spot on board

(define BD0 (list O O 
                  O O))

(define BD0s (list Q O
                   O O))

(define BD1 (list O O O O
                  O O O O
                  O O O O
                  O O O O))

(define BD1a (list O Q O O
                   O O O O
                   O O O O
                   O O O O))

(define BD1s (list O Q O O
                   O O O Q
                   Q O O O
                   O O Q O))


;; ===========
;; Functions:

(@htdf main)
(@signature Natural -> Image)
;; renders a solved nqueens board
;; Entry point of program
;; <no tests for main>

(@template use-abstract-fn fn-composition)

(define (main n)
  (above  (text "Before:" 24 "white")
          (print-board (build-list (sqr n) (λ (n) #f)))
          (text "After:" 24 "white")
          (local [(define try (solve n))]
            (if (not (false? try))
                (print-board (solve n))
                (text "error: UNSOLVABLE" 24 "red")))))

(@htdf solve)
(@signature Natural -> Board)
;; produce a board that n*n big that solves the n queens problem
;; CONSTRAINT: n must be > 0
(check-expect (solve 1) (list Q))
(check-expect (solve 4) BD1s)
(check-expect (solve 2) false)

(@template encapsulated arb-tree genrec try-catch)

(define (solve n)
  (local [(define (solve--bd bd)
            (if (solved? bd)
                bd
                (solve--lobd (next-boards bd))))

          (define (solve--lobd lobd)
            (cond [(empty? lobd) false]
                  [else
                   (local [(define try (solve--bd (first lobd)))]
                     (if (not (false? try))
                         try
                         (solve--lobd (rest lobd))))]))]
    
    (solve--bd (build-list (sqr n) (λ (n) #f)))))



(@htdf solved?)
(@signature Board -> Boolean)
;; produce true if (sqrt (length bd)) queens exist on bd
(check-expect (solved? BD0) false)
(check-expect (solved? BD0s) false)
(check-expect (solved? BD1) false)
(check-expect (solved? BD1a) false)
(check-expect (solved? BD1s) true)

;; Problem solving approach:
;; 1. Filter board for only true elements
;; 2. Map each true element to the number 1
;; 3. Foldr and add each element in list
;; 4. Check if foldr result = (sqrt (length bd))

(@template use-abstract-fn fn-composition) 

(define (solved? bd)
  (local [(define n (sqrt (length bd)))]
    (= (foldr + 0 (map (λ (e) 1) (filter (compose not false?) bd))) n)))


(@htdf next-boards)
(@signature Board -> (listof Board))
;; produce list of next valid boards
;; ASSUME board is not already solved

(check-expect (next-boards BD0) (list BD0s
                                      (list #f #t #f #f)
                                      (list #f #f #t #f)
                                      (list #f #f #f #t)))
(check-expect (next-boards BD0s) empty)
(check-expect (next-boards BD1) (build-list (length BD1)
                                            (λ (p) (fill-cell BD1 p #t))))
(check-expect (next-boards BD1a) (list (fill-cell BD1a 7 #t)
                                       (fill-cell BD1a 8 #t)
                                       (fill-cell BD1a 10 #t)
                                       (fill-cell BD1a 12 #t)
                                       (fill-cell BD1a 14 #t)
                                       (fill-cell BD1a 15 #t)))

(check-expect (next-boards BD1s) empty)

;; Problem solving approach:
;; 1. Create list of empty positions
;; 2. Create board for each empty position,
;;    filling each with true.
;; 3. Prune list of boards for only valid ones

(@template fn-composition)

;; could potentially put find-empty-positions in local
;; and then pass into valid boards?

(define (next-boards bd)
  (valid-boards (generate-boards (find-positions false? bd 0) bd)))



(@htdf find-positions)
(@signature (Boolean -> Boolean) Board Natural -> (listof Position))
;; produce list of all positions in board that satisfy predicate element
(check-expect (find-positions false? (list #t #t #t #t) 0) empty)
(check-expect (find-positions false? BD0 0) (list 0 1 2 3))
(check-expect (find-positions false? BD1 0) (list 0 1 2 3 4 5 6 7 8 9 10
                                                  11 12 13 14 15))
(check-expect (find-positions false? BD1s 0) (list 0 2 3 4 5 6 9 10 11 12
                                                   13 15))
(check-expect (find-positions (compose not false?) BD1a 0) (list 1))
(check-expect (find-positions (compose not false?) BD0 0) empty)
(check-expect (find-positions (compose not false?) BD1s 0) (list 1 7 8 14)) 

(@template Board)

(define (find-positions fn bd n)
  (cond [(empty? bd) empty]
        [else
         (if (fn (first bd))
             (cons n (find-positions fn (rest bd) (add1 n)))
             (find-positions fn (rest bd) (add1 n)))]))


(@htdf generate-boards)
(@signature (listof Position) Board -> (listof Board))
;; produce a list of boards with one new spot filled in
(check-expect (generate-boards (list 0 1 2 3) BD0) (list (list #t #f #f #f)
                                                         (list #f #t #f #f)
                                                         (list #f #f #t #f)
                                                         (list #f #f #f #t)))
(check-expect (generate-boards (list 0 2 3 4 5 6 7 8 9 10 11 12 13 14 15) BD1a)
              (list (fill-cell BD1a 0 #t) (fill-cell BD1a 2 #t)
                    (fill-cell BD1a 3 #t) (fill-cell BD1a 4 #t)
                    (fill-cell BD1a 5 #t) (fill-cell BD1a 6 #t)
                    (fill-cell BD1a 7 #t) (fill-cell BD1a 8 #t)
                    (fill-cell BD1a 9 #t) (fill-cell BD1a 10 #t)
                    (fill-cell BD1a 11 #t) (fill-cell BD1a 12 #t)
                    (fill-cell BD1a 13 #t) (fill-cell BD1a 14 #t)
                    (fill-cell BD1a 15 #t)))
                                           

;; Problem solving approach:
;; 1. Use build-list to generate a list of boards where at each of
;;    the given positions, a true is placed within.
;;    (leverage the natural to access index of list)

(@template use-abstract-fn)

(define (generate-boards lop bd)
  (build-list (length lop) (λ (p) (fill-cell bd (list-ref lop p) #t))))



(@htdf valid-boards)
(@signature (listof Board) -> (listof Board))
;; produce listof boards of which no queen attacks another
(check-expect (valid-boards (list BD0 BD1 BD1a))
              (list BD0 BD1 BD1a))
(check-expect (valid-boards (list BD1a (fill-cell BD1a 3 #t)))
              (list BD1a))
(check-expect (valid-boards (list (fill-cell BD1a 11 #t) (fill-cell BD1a 9 #t)))
              empty)
(check-expect (valid-boards (list (fill-cell BD1a 8 #t)))
              (list (fill-cell BD1a 8 #t)))
(check-expect (valid-boards (list (fill-cell BD1a 4 #t) (fill-cell BD1a 5 #t)
                                  (fill-cell BD1a 14 #t)))
              (list (fill-cell BD1a 14 #t)))

;; Problem solving approach:
;; 1. Filter lobd using predicate valid-board?

(@template use-abstract-fn)

(define (valid-boards lobd)
  (filter valid-board? lobd))


(@htdf valid-board?)
(@signature Board -> Boolean)
;; produce true if board has queens that are not attacking each other
(check-expect (valid-board? BD0) true)
(check-expect (valid-board? BD1) true)
(check-expect (valid-board? BD1a) true)
(check-expect (valid-board? (fill-cell BD1a 14 #t)) true)
(check-expect (valid-board? (fill-cell BD1a 11 #t)) false)
(check-expect (valid-board? (fill-cell BD1a 9 #t)) false)
(check-expect (valid-board? (fill-cell BD1a 3 #t)) false)
(check-expect (valid-board? (fill-cell BD1a 8 #t)) true)
(check-expect (valid-board? (fill-cell BD1a 4 #t)) false)
(check-expect (valid-board? (fill-cell BD1a 5 #t)) false)

;; Problem solving approach:
;; We need to find positions of each true element in board.
;; 1. Regenerate list of positions for each true element
;; 2. For each position, andmap and compare the x's, y's, and slope.
;; andmap each true element with primitive being attack (one after another)

(@template fn-composition use-abstract-fn)

(define (valid-board? bd)
  (local [; ==========
          ; Constants:
          
          (define lop (find-positions (compose not false?) bd 0))
          
          ; ========
          ; Functions:
          ;
          ; (@signature Position -> Boolean)
          ; produce true if no position after it in lop attacks the queen
          ; (@template use-abstract-fn)
          
          (define (valid-pos? pa)
            (local [; (@signature Position -> Boolean)
                    ; produce true if pa or pb attack each other
                    ; (@template Natural)
                    (define (attack? pb)
                      (local [(define pa-x (pos->x pa bd))
                              (define pa-y (pos->y pa bd))
                              (define pb-x (pos->x pb bd))
                              (define pb-y (pos->y pb bd))]
                        
                      (and (not (= pa pb))
                           (or (= pa-x pb-x)                       ;same row
                               (= pa-y pb-y)                       ;same column
                               (= (abs (/ (- pb-y pa-y) (- pb-x pa-x))) 1)))))] 
                                                                 ; slope is 1/-1
            (not (ormap attack? lop))))]
              
    (andmap valid-pos? lop)))










;; ------- |
;; Helpers |
;; ------- |

(@htdf pos->x)
(@signature Position Board -> Natural)
;; produce x-coordinate from given pos according to board
;; ASSUME given position complys with board
(check-expect (pos->x 3 BD0) 1)
(check-expect (pos->x 0 BD0) 0)
(check-expect (pos->x 11 BD1) 3)
(check-expect (pos->x 12 BD1) 0)

(@template Position)

(define (pos->x p bd)
  (local [(define n (sqrt (length bd)))]
    (remainder p n)))


(@htdf pos->y)
(@signature Position Board -> Natural)
;; produce y-coordinate from given pos according to board
;; ASSUME given position complys with board
(check-expect (pos->y 3 BD0) 1)
(check-expect (pos->y 0 BD0) 0)
(check-expect (pos->y 11 BD1) 2)
(check-expect (pos->y 12 BD1) 3)

(@template Position)

(define (pos->y p bd)
  (local [(define n (sqrt (length bd)))]
    (quotient p n)))

 
(@htdf x-y->pos)
(@signature Natural Natural Board -> Position)
;; convert cartesian coordinates to natural that can access board flat-list
;; ASSUME (x,y) position complys with board
(check-expect (x-y->pos 0 0 BD0) 0)
(check-expect (x-y->pos 1 1 BD0) 3)
(check-expect (x-y->pos 3 2 BD1) 11)
(check-expect (x-y->pos 0 3 BD1) 12) 

(@template Natural)

(define (x-y->pos x y bd)
  (local [(define s (sqrt (length bd)))]
    (+ (* y s) x)))


(@htdf get-cell)
(@signature Position Board -> Boolean)
;; produce value at given position in board
(check-expect (get-cell 0 BD1s) #f)
(check-expect (get-cell 1 BD1a) #t)
(check-expect (get-cell 15 BD1s) #f)
(check-expect (get-cell 14 BD1s) #t)

(@template 2-one-of)

#|
 bd>               empty      (cons Boolean Board)
p
v

0                  INVALID    (first bd) ; (1)

(add1 Natural)     INVALID    (get-cell (sub1 p) (rest bd)) ; (2)

|#

(define (get-cell p bd)
  (cond [(zero? p) (first bd)]
        [else
         (get-cell (sub1 p) (rest bd))]))



(@htdf fill-cell)
(@signature Board Position Boolean -> Board)
;; produce new board with given value filled at given pos in bd.
(check-expect (fill-cell BD1 0 #t) (cons #t (rest BD1)))
(check-expect (fill-cell BD0 1 #f) BD0)
(check-expect (fill-cell BD0 1 #t) (cons #f (cons #t (rest (rest BD0)))))

(@template 2-one-of)

#|
 bd>               empty      (cons Boolean Board)
p
v

0                  INVALID    (cons nv (rest bd)) ; (1)

(add1 Natural)     INVALID    (cons (first bd)    ; (2)
                                    (fill-cell (rest bd) (sub1 p) nv))
|#

(define (fill-cell bd p nv)
  (cond [(zero? p) (cons nv (rest bd))]; (1)
        [else                          ; (2)
         (cons (first bd)
               (fill-cell (rest bd) (sub1 p) nv))]))





(@htdf x-y-ref)
(@signature Natural Natural Board -> Boolean)
;; produce value at given x,y coordinates in board
;; CONSTRAINT: position must be in bounds with given board
(check-expect (x-y-ref 0 0 BD0) #f)
(check-expect (x-y-ref 1 0 BD1a) #t)
(check-expect (x-y-ref 3 1 BD1s) #t)
(check-expect (x-y-ref 2 3 BD1s) #t)
(check-expect (x-y-ref 3 3 BD1s) #f)

(@template fn-composition)

(define (x-y-ref x y bd)
  (get-cell (x-y->pos x y bd) bd))




;; ================
;; Rendering functionality:

(define SQ-SIZE 20)
(define ELT-SIZE 15)
(define VAL-COLOR "yellow")
(define F-COLOR "red")


(@htdf print-board)
(@signature Board -> Image)
;; Abstraction layer for display-board
;; No tests needed

(@template use-abstract-fn)

(define (print-board bd)
  (display-board bd 0))


(@htdf display-board)
(@signature Board Natural -> Image)
;; produces a image representation of a chess board, with Queens denoted by Q
;; <tests elided>

(@template Natural)
 
(define (display-board bd n)
  (cond [(= n (sqrt (length bd))) empty-image]
        [else
         (above (beside (generate-square (get-cell (* n (sqrt (length bd))) bd))
                        (layout-images bd (add1 (* n (sqrt (length bd))))))
                (display-board bd (add1 n)))]))

(@htdf layout-images)
(@signature Board Natural -> Image)
;; produces a row of chess cells in given board starting from given position
;; <tests elided>

(@template Natural)

(define (layout-images bd n)
  (cond [(zero? (remainder n (sqrt (length bd)))) empty-image]
        [else
         (beside (generate-square (get-cell n bd))
                 (layout-images bd (add1 n)))]))
             



(@htdf generate-square)
(@signature Boolean -> Image)
;; produce square in nqueens board
(check-expect (generate-square false) (overlay
                                       (square SQ-SIZE "outline" "white")
                                       (text " " ELT-SIZE F-COLOR)))
(check-expect (generate-square true) (overlay
                                   (square SQ-SIZE "outline" "white")
                                   (text "Q" ELT-SIZE VAL-COLOR)))

(@template Boolean)

(define (generate-square b)
  (cond [(false? b) (overlay (square SQ-SIZE "outline" "white")
                             (text " " ELT-SIZE F-COLOR))]
        [else
         (overlay
          (square SQ-SIZE "outline" "white")
          (text "Q" ELT-SIZE VAL-COLOR))]))