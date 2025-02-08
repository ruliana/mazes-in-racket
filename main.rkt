#lang racket/base


(require
  racket/function
  racket/string
  racket/generic
  racket/match
  racket/random
  racket/generator
  racket/set
  racket/trace
  threading
  data/collection
  "var.rkt"
  "svg.rkt")

(require (for-syntax racket/base syntax/parse))

(module+ test
  (require rackunit))


;; Utilities and redefinitions
;; These utility functions and redefinitions are used to simplify operations
;; throughout the code, providing shorthand and more efficient ways to handle
;; common tasks like vector manipulation and probability checks.
(define (make-immutable-vector size initial-element)
  (vector->immutable-vector
   (make-vector size initial-element)))

(define ivector vector-immutable)

(define (update-nth** vec i updater)
  (if i (update-nth vec i updater) vec))

(define (probability? n)
  (< (random) n ))

(define (clip minimum x maximum)
  (max minimum (min maximum x)))

(define-syntax (define* stx)
  (syntax-parse stx
    [(_ id:id [(elems ...) body ...+] ...+)
     #'(define id
         (match-lambda*
           [(list elems ...) body ...] ...))]))  

;; === Maze ===
;; The maze is represented as a grid of cells, each with walls that can be
;; opened or closed. The maze generation logic is built around manipulating
;; these walls to create paths.

(define (pos r c) (make-rectangular c r))
;; Positions in the maze are represented as complex numbers, where the real
;; part is the column and the imaginary part is the row. This allows for
;; concise arithmetic operations to navigate the maze.

(define row imag-part)
(define col real-part)

(define (lf p) (sub1 p))
(define (rg p) (add1 p))
(define (up p) (+ p 0-1i))
(define (dn p) (+ p 0+1i))

;; Grid is a vector of integers, each bit represents a wall (1) or an opening (0)
;; The bit order is right to left, counter-clockwise, starting with north:
;; This encoding allows for efficient manipulation of cell walls using bitwise
;; operations, which is crucial for the maze generation process.
;; North = 1110 = 14
;; West = 1101 = 13
;; South = 1011 = 11
;; East = 0111 = 7

(define cell-closed #b1111)
(define cell-hollow #b0000)

(struct maze (dim content)  #:transparent)
;; The maze structure holds the dimensions and content of the maze, where
;; content is a vector representing the state of each cell's walls.

(define* get
;; Retrieves the value of a cell in the maze, using either a position or
;; row and column indices. This function abstracts the complexity of
;; accessing the maze's underlying vector representation.
  
  [(mz pos)
   (var i (->index mz pos)
        v (maze-content mz))
   (and i (ref v i))]
  
  [(mz row col)
   (get mz (pos row col))])


(define* set
;; Updates the value of a cell in the maze, with the ability to modify
;; adjacent cells based on the current cell's state. This function is
;; central to the maze generation process, as it allows for dynamic
;; carving and blocking of paths.

  [(mz pos value)
   (define val (if (procedure? value)
                   (value (get mz pos))
                   value))
   (define (update vec p test? then else)
     (var i (->index mz p)
          updater (if (test? val) then else))
     (update-nth** vec i updater))
   (define new-content
     (~> (maze-content mz)
         (update pos (const #t) (const val) identity)
         (update (up pos) wall-north? block-south carve-south)
         (update (dn pos) wall-south? block-north carve-north)
         (update (lf pos) wall-west? block-east carve-east)
         (update (rg pos) wall-east? block-west carve-west)))
   (struct-copy maze mz [content new-content])]

  [(mz row col value)
   (set mz (pos row col) value)])

(define (make-maze rows cols)
;; Initializes a new maze with all cells closed. This function sets up
;; the initial state of the maze before any paths are carved.
  (var starting-vec (make-immutable-vector (* rows cols) cell-closed))
  (maze (pos rows cols) starting-vec))

(define (maze-rows mz)
  (~> mz maze-dim row))

(define (maze-cols mz)
  (~> mz maze-dim col))

(define (last-row mz)
  (sub1 (maze-rows mz)))

(define (last-col mz)
  (sub1 (maze-cols mz)))

(define (at-last-row? mz p)
  (= (row p) (last-row mz)))

(define (at-last-col? mz p)
  (= (col p) (last-col mz)))


(define ((wall-checker wall-pos) cell)
;; Checks if a specific wall is present in a cell. This is used to
;; determine the current state of a cell's walls during the maze
;; generation process.
  (and cell (bitwise-bit-set? cell wall-pos)))

(define wall-north? (wall-checker 0))
(define wall-west? (wall-checker 1))
(define wall-south? (wall-checker 2))
(define wall-east? (wall-checker 3))


(define ((carver template) cell)
;; Carves a path by removing specific walls from a cell. This function
;; is used to create openings in the maze, allowing for path creation.
  (and cell (bitwise-and template cell)))

(define carve-north (carver #b1110))
(define carve-west (carver #b1101))
(define carve-south (carver #b1011))
(define carve-east (carver #b0111))


(define ((blocker template) cell)
;; Blocks a path by adding specific walls to a cell. This function is
;; used to close off paths, ensuring that the maze has a defined structure.
  (and cell (bitwise-ior template cell)))

(define block-north (blocker #b0001))
(define block-west (blocker #b0010))
(define block-south (blocker #b0100))
(define block-east (blocker #b1000))


(define (->index mz p)
  (var valid? (and (< -1 (row p) (maze-rows mz))
                   (< -1 (col p) (maze-cols mz))))
  (and valid? (+ (col p) (* (row p) (maze-cols mz)))))


(define (print-maze mz)
;; Prints a textual representation of the maze, using characters to
;; represent walls and openings. This is useful for debugging and
;; visualizing the maze's structure.
  (var max-rows (maze-rows mz)
       max-cols (maze-cols mz))
  (displayln (make-string (add1 (* 3 max-cols)) #\-))
  (for ([r max-rows])
    (define x
      (for/list ([c max-cols])
        (if (wall-east? (get mz r c)) "  |" "   ")))
    (define y
      (for/list ([c max-cols])
        (if (wall-south? (get mz r c)) "--+" "  +")))
    (displayln (string-join x "" #:before-first "|"))
    (displayln (string-join y "" #:before-first "+"))))


(define (svg-maze mz)
;; Generates an SVG representation of the maze, allowing for a visual
;; depiction of the maze's structure. This is useful for exporting and
;; sharing the maze in a graphical format.
  (var cell-size 10 ;; Must be 10 because that's how I drew them in svg
       margin 2
       scale 2
       rows (maze-rows mz)
       cols (maze-cols mz)
       img-width (+ (* scale 2 margin) (* scale cols cell-size))
       img-height (+ (* scale 2 margin) (* scale rows cell-size)))
  (var body (for*/list ([r (in-range (maze-rows mz))]
                        [c (in-range (maze-cols mz))])
              (var cell (ref svg-cells (get mz r c))
                   t (format "translate(~a, ~a)" (* cell-size c) (* cell-size r)))
              `(g ((transform ,t)) ,cell)))
  (svg img-width img-height
       (list
        '(style "path {fill: none; stroke: black; stroke-width: 1; stroke-linejoin: round} #background {fill: ghostwhite}")
        `(g ((transform ,(format "scale(~a)" scale)))
            (rect ((id "background")
                   (x ,(format "~a" (* 0.5 margin)))
                   (y ,(format "~a" (* 0.5 margin)))
                   (width ,(number->string (+ margin (* cell-size cols))))
                   (height ,(number->string (+ margin (* cell-size rows))))
                   (stroke "black")))       
            (g ((transform ,(format "translate(~a, ~a)" margin margin)))
               ,@body)))))

(define (save-svg-maze filename mz)
  (with-output-to-file
    filename
    (λ () (~> mz svg-maze svg-show))
    #:exists 'replace))


;; ====================
;; Sidewinder algorithm
;; ====================

;; Block carvers for sidewinder algorithm
;; These functions define different strategies for carving paths in the maze
;; using the Sidewinder algorithm. Each strategy has its own approach to
;; selecting which cells to carve, affecting the maze's final structure.
;; A block carver is a function that takes a maze, a block and a carver
;; and carves the block with the carver. The block is a list of positions
;; and the carver is a function that takes a maze and a position and carves
;; the maze at that position.
(define ((block-carver-random carver) mz block)
  (if (empty? block) mz
      (let ([random-pos (random-ref block)])
        (set mz random-pos carver))))

(define ((block-carver-fixed carver index) mz block)
  (var abs-index (if (negative? index)
                   (+ (length block) index)
                   index)
       i (clip 0 abs-index (sub1 (length block)))
       p (ref (reverse block) i))
  (if (empty? block) mz
      (set mz p carver)))

(define ((block-carver-relative carver ratio) mz block)
  (if (empty? block) mz
      (let ([index (inexact->exact (ceiling (* ratio (length block))))])
        ((block-carver-fixed carver (sub1 index)) mz block))))

(define ((block-carver-no-vertical carver wall-checker?) mz block)
  (if (empty? block) mz
      (let loop ([attempts 3])
        (var random-pos (random-ref block)
             cell (get mz random-pos))
        (if (or (zero? attempts) (wall-checker? cell))
            (set mz random-pos carver)
            (loop (sub1 attempts))))))

;; Parameter: which block carver to use
(define sidewinder-block-carver
  (make-parameter (block-carver-random carve-south)))


;; Side conditions for sidewinder algorithm
;; These functions define conditions under which the carving process should
;; continue or stop. They introduce variability and randomness into the maze
;; generation, ensuring that each maze is unique.
;; A side condition is a function that takes a maze, a position and a block
;; and returns a boolean indicating whether to continue carving in that direction
(define ((carve-condition-always) mz p block)
  #t)

(define ((carve-condition-random probability) mz p block)
  (probability? probability))

(define ((carve-condition-row-alternate . conditions) mz p block)
  (var r (row p)
       index (modulo r (length conditions))
       condition? (list-ref conditions index))
  (condition? mz p block))

(define ((carve-condition-odd probability) mz p block)
  (or (probability? probability)
      (odd? (length block))))

(define ((carve-condition-even probability) mz p block)
  (or (probability? probability)
      (even? (length block))))

;; Parameter: probability of continue carving
(define sidewinder-side-condition (make-parameter (carve-condition-random 0.5)))


(define (sidewinder mz)
;; Implements the Sidewinder algorithm to generate a maze. This algorithm
;; carves paths by moving east with a certain probability and carving south
;; at random intervals, creating a maze with a distinct pattern.
  ;; Carve east with a certain probability
  ;; If not carve east, carve south in a random cell of the current block
  ;; Start a new block
  (var block-carver (sidewinder-block-carver)
       continue-carving? (sidewinder-side-condition))
  (define (carve-whole-line mz)
    (for/fold ([mz mz])
              ([c (in-range (last-col mz))])
      (set mz (last-row mz) c carve-east)))
  (let loop ([mz mz]
             [p (pos 0 0)]
             [block (list)])
    (var new-block (conj block p)
         next-line (pos (add1 (row p)) 0)
         right (rg p))
    (cond
      ;; If last row, carve the whole line east
      ;; and return
      [(at-last-row? mz p)
          (carve-whole-line mz)]
      ;; If last column, carve the block south
      ;; and next line
      [(at-last-col? mz p)
       (loop (block-carver mz new-block)
             next-line
             (list))]
      ;; Given a condition, carve east
      [(continue-carving? mz p block)
       (loop (set mz p carve-east)
             right
             new-block)]
      ;; Otherwise, carve south in a random cell of
      ;; the accumulated block so far
      [else
       (loop (block-carver mz new-block)
             right
             (list))])))


(define (sidewinder-around mz)
;; An alternative maze generation approach that carves paths around the
;; perimeter of the maze. This function explores different carving strategies
;; to create varied maze structures.
  (define (in-bound? mz p)
    (and (< -1 (row p) (maze-rows mz))
         (< -1 (col p) (maze-cols mz))))
  (define out-bound? (negate in-bound?))
  (define (visited? v e) (member e v))
  (define (turn-right direction)
    (var turner (hasheq
                 rg dn
                 dn lf
                 lf up
                 up rg))
    (ref turner direction))
  (define (carve mz p direction)
    (var carver (hasheq
                  rg carve-east
                  dn carve-south
                  lf carve-west
                  up carve-north))
    (set mz p (ref carver direction)))
  (define (carve-to-right mz block)
    (var (list p dir) (random-ref block))
    (carve mz p (turn-right dir)))
  (var start (pos 0 0))
  (let loop ([mz mz]
             [p start]
             [direction rg]
             [block (list (list start rg))]
             [visited (list start)])
    (var next-pos (direction p)
         next-cell-dir (list next-pos direction))
    (cond
      ;; All visited
      [(= (length visited) (length (maze-content mz)))
       mz]
      [(or (out-bound? mz next-pos)
           (visited? visited next-pos))
       (loop mz p (turn-right direction) (rest block) visited)]
      [(probability? 0.8)
       (loop (carve mz p direction)
             next-pos
             direction
             (conj block next-cell-dir)
             (conj visited next-pos))]
      [else #t
       (loop (carve-to-right mz block)
             next-pos
             direction
             (list next-cell-dir)
             (conj visited next-pos))])))
      
    
             
  
(module+ test
  (test-case
   "get"
   (define mz (maze (pos 3 3) (ivector 1 2 3 4 5 6 7 8 9)))
   (check-equal? (get mz 0 0) 1)
   (check-equal? (get mz 2 2) 9)
   (check-equal? (get mz 1 1) 5)
   (check-equal? (get mz -1 0) #f)
   (check-equal? (get mz 0 -1) #f)
   (check-equal? (get mz 10 0) #f)
   (check-equal? (get mz 0 10) #f))
  (test-case
   "set"
     (define full-maze (make-maze 3 3))
     (define empty-maze (maze (pos 3 3) (ivector 0 0 0 0 0 0 0 0 0)))
     (check-equal? (maze-content (set full-maze 1 1 cell-hollow))
                   (ivector 15 11 15 7 0 13 15 14 15))
     (check-equal? (maze-content (set empty-maze 1 1 cell-closed))
                   (ivector 0 4 0 8 15 2 0 1 0))
     (check-equal? (maze-content (~> (make-maze 3 3) (set 1 1 carve-north) (set 1 2 carve-north)))
                   (ivector 15 11 11 15 14 14 15 15 15))
     (check-equal? (maze-content (~> (make-maze 3 3) (set (pos 0 0) carve-east)))
                   (ivector #b0111 #b1101 15 15 15 15 15 15 15))))


(module+ main
;; Command-line interface for generating and saving a maze as an SVG file.
;; This allows users to specify maze dimensions and carving probabilities,
;; providing flexibility in the maze generation process.
  (require racket/cmdline)
  (command-line
    #:program "maze"
    #:args (filename row col side-probability)
    (parameterize ([sidewinder-block-carver
                    (block-carver-relative carve-south 0.5)
                    #;(block-carver-no-vertical carve-south wall-north?)]
                   [sidewinder-side-condition
                    (carve-condition-odd (string->number side-probability))
                    #;(carve-condition-row-alternate
                     (carve-condition-even (string->number side-probability))
                     (carve-condition-even (string->number side-probability))
                     (carve-condition-always))])
      (var mz (make-maze (string->number row) (string->number col))
           rslt (sidewinder mz))
      (save-svg-maze filename rslt))))
