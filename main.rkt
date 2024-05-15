#lang racket/base


(require
  racket/function
  racket/string
  racket/generic
  racket/match
  racket/random
  racket/generator
  racket/trace
  threading
  data/collection
  "var.rkt"
  "svg.rkt")

(require (for-syntax racket/base syntax/parse))

(module+ test
  (require rackunit))


;; Utilities and redefinitions
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

(struct pos (row col) #:transparent)

(define (up p) (struct-copy pos p [row (~> p pos-row sub1)]))
(define (dn p) (struct-copy pos p [row (~> p pos-row add1)]))
(define (lf p) (struct-copy pos p [col (~> p pos-col sub1)]))
(define (rg p) (struct-copy pos p [col (~> p pos-col add1)]))

;; Grid is a vector of integers, each bit represents a wall (1) or an opening (0)
;; The bit order is right ot left, counter-clockwise, strating with north:
;; North = 1110 = 14
;; West = 1101 = 13
;; South = 1011 = 11
;; East = 0111 = 7

(define cell-closed #b1111)
(define cell-hollow #b0000)

(struct maze (pos content)  #:transparent)

(define* get
  
  [(mz pos)
   (var i (->index mz pos)
        v (maze-content mz))
   (and i (ref v i))]
  
  [(mz row col)
   (get mz (pos row col))])


(define* set

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
  (var starting-vec (make-immutable-vector (* rows cols) cell-closed))
  (maze (pos rows cols) starting-vec))

(define (maze-rows mz)
  (var (maze (pos r _) _) mz)
  r)

(define (maze-cols mz)
  (var (maze (pos _ c) _) mz)
  c)

(define (last-row mz)
  (sub1 (maze-rows mz)))

(define (last-col mz)
  (sub1 (maze-cols mz)))

(define (at-last-row? mz p)
  (= (pos-row p) (last-row mz)))

(define (at-last-col? mz p)
  (= (pos-col p) (last-col mz)))

(define ((wall-checker wall-pos) cell)
  (and cell (bitwise-bit-set? cell wall-pos)))

(define wall-north? (wall-checker 0))
(define wall-west? (wall-checker 1))
(define wall-south? (wall-checker 2))
(define wall-east? (wall-checker 3))


(define ((carver template) cell)
  (and cell (bitwise-and template cell)))

(define carve-north (carver #b1110))
(define carve-west (carver #b1101))
(define carve-south (carver #b1011))
(define carve-east (carver #b0111))


(define ((blocker template) cell)
  (and cell (bitwise-ior template cell)))

(define block-north (blocker #b0001))
(define block-west (blocker #b0010))
(define block-south (blocker #b0100))
(define block-east (blocker #b1000))


(define (->index mz p)
  (var (pos row col) p
       (maze maxes v) mz
       (pos max-row max-col) maxes
       valid? (and (< -1 row max-row)
                   (< -1 col max-col)))
  (and valid? (+ col (* row max-col))))


(define (print-maze mz)
  (var (maze (pos max-rows max-cols) _) mz)
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
;; A side condition is a function that takes a maze, a position and a block
;; and returns a boolean indicating whether to continue carving in that direction
(define ((carve-condition-always) mz p block)
  #t)

(define ((carve-condition-random probability) mz p block)
  (probability? probability))

(define ((carve-condition-row-alternate . conditions) mz p block)
  (var row (pos-row p)
       index (modulo row (length conditions))
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
         next-line (pos (add1 (pos-row p)) 0)
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
  (require racket/cmdline)
  (command-line
    #:program "maze"
    #:args (filename row col side-probability)
    (parameterize ([sidewinder-block-carver
                    (block-carver-no-vertical carve-south wall-north?)]
                   [sidewinder-side-condition
                    (carve-condition-row-alternate
                     (carve-condition-even (string->number side-probability))
                     (carve-condition-even (string->number side-probability))
                     (carve-condition-always))])
      (var mz (make-maze (string->number row) (string->number col))
           rslt (sidewinder mz))
      (save-svg-maze filename rslt))))
    
    
