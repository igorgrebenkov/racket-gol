#lang racket

; Implements the game logic

(require "global-vars.rkt")
(provide (all-defined-out))

; Initializes the cell hash tables
;
; Initially, cell-ht only contains alive cells selected by the user.
; The hash table ht0 provided holds the keys of all alive cells
; and their neighbors. Hence, if any of those neighbors are dead,
; they won't be in cell-ht yet, and so we add them.
;
; Otherwise, if cell-ht does contain an element with that key, it
; must be a live cell, and so we copy it to cell-buf. This has to
; be done so that cells that don't change state won't be lost when
; assigning cell-buf back to cell-ht at the end of each generation.
(define (init-ht ht0 ht1 ht2)
  (for ([(key value) (in-hash ht0)])
    (cond ((hash-has-key? ht1 key)
           (hash-set! ht2 key (hash-ref ht1 key)))
          (else
           (hash-set! ht1 key DEAD)))))

; Generates a list of the Moore neighborhood of a cell (toroidal)
(define (cell-neighbors-moore-toroid key)
  (let* ((x (car key))
         (y (cadr key))
         (top-left (list (modulo (- x 1) max-x) (modulo (- y 1) max-y)))
         (top (list (modulo x max-x) (modulo (- y 1) max-y)))
         (top-right (list (modulo (+ x 1) max-x) (modulo (- y 1) max-y)))
         (left (list (modulo (- x 1) max-x) (modulo y max-y)))
         (right (list (modulo (+ x 1) max-x) (modulo y max-y)))
         (bottom-left (list (modulo (- x 1) max-x) (modulo (+ y 1) max-y)))
         (bottom (list (modulo x max-x) (modulo (+ y 1) max-y)))
         (bottom-right (list (modulo (+ x 1) max-x) (modulo (+ y 1) max-y))))
    (list top-left
          top
          top-right
          left
          right
          bottom-left
          bottom
          bottom-right)))

; Generates a list of the Moore neighborhood of a cell (infinite)
(define (cell-neighbors-moore key)
  (let* ((x (car key))
         (y (cadr key))
         (top-left (list (- x 1) (- y 1)))
         (top (list x (- y 1)))
         (top-right (list (+ x 1) (- y 1)))
         (left (list (- x 1) y))
         (right (list (+ x 1) y))
         (bottom-left (list (- x 1) (+ y 1)))
         (bottom (list x (+ y 1)))
         (bottom-right (list (+ x 1) (+ y 1))))
    (list top-left
          top
          top-right
          left
          right
          bottom-left
          bottom
          bottom-right)))

; Get a hash table of alive cells and their neighbors
(define (cell-active ht)
  (let ((result (make-hash)))
    (for ([(key value) (in-hash ht)]
           #:when (equal? value ALIVE))
      (cond ((hash-has-key? cell-neighbor key)
             (hash-set! result key value)
             (for ([i (hash-ref cell-neighbor key)])
               (hash-set! result i DEAD)))
            (else
             (let ((neighbors (cell-neighbors-moore key)))
               (hash-set! cell-neighbor key neighbors)
               (for ([i neighbors])
                 (hash-set! result i DEAD))))))
    result))

; Updates the state of a cell based on its neighbors
(define (cell-next-gen key status num-alive ht-buf)
    (cond ((equal? status ALIVE)
           (cond ((or (< num-alive alive-lower-lim)
                      (> num-alive alive-upper-lim))
                  (hash-set! ht-buf key DEAD)))) 
          (else
           (equal? status DEAD)
           (cond ((equal? num-alive dead-thresh)
                  (hash-set! ht-buf key ALIVE))))))

; Counts the number of alive cells in a cell's neighborhood
(define (cell-neighbor-count ht key neighbors)
  (let ((alive-count 0))
    (for ([neighbor-key neighbors])
      (cond ((hash-has-key? ht neighbor-key)
             (let ((neighbor-value (hash-ref ht neighbor-key)))
               (set! alive-count (+ alive-count neighbor-value))))))
    alive-count))

; Calculates the next generation of the board
(define (board-next-gen ht ht-buf) 
  (for ([(key value) (in-hash ht)])
    (let* ((neighbors (cond ((hash-has-key? cell-neighbor key)
                             (hash-ref cell-neighbor key))
                            (else
                             (cell-neighbors-moore key))))
           (alive-count (cell-neighbor-count cell-ht key neighbors)))
      (cell-next-gen key value alive-count ht-buf))))

; Returns a hash table of elements in ht2 with different
; values for elements of the same key in ht1
(define (hash-diff ht1 ht2)
  (for/hash ([(key value) (in-hash ht2)]
             #:when (not (equal? value (hash-ref ht1 key))))
    (values key value)))

; Returns new hash table with only alive cells remaining
(define (cell-remove-dead ht)
  (for/hash ([(key value) (in-hash ht)]
             #:when (equal? value ALIVE))
    (values key value)))

; Trims a hash table to remove keys that are less
; than the current x and y maxes (for window resizing)
(define (cell-trim ht)
  (let ((new-hash (make-hash)))
    (for ([(key value) (in-hash ht)]
          #:when (and (< (car key) max-x)
                      (< (cadr key) max-y)))
      (hash-set! new-hash key value))
    new-hash))

; Randomly seeds the hash table
(define (cell-seed ht)
  (for ([i (in-range 0 max-x)])
    (for ([j (in-range 0 max-y)])
    (cond ((equal? (random 12) 0)
           (hash-set! ht (list i j) ALIVE))))))

; Change keys by adding offset of x and y
; Used to reposition cells when zooming in/out on the board
(define (cell-offset ht x y)
  (let ((new-hash (make-hash)))
    (for ([(key value) (in-hash ht)])
      (let ((new-x (+ (car key) x))
            (new-y (+ (cadr key) y)))
        (cond ((and (<= (abs new-x) max-x)
                    (<= (abs new-y) max-y))
               (hash-set! new-hash (list new-x new-y) value)))))
    new-hash))



; Gosper gun hash table
(define gosper '#hash(((67 34) . 1) ((67 33) . 1) ((62 32) . 1)
                      ((86 32) . 1) ((66 32) . 1) ((72 32) . 1)
                      ((85 31) . 1) ((61 33) . 1) ((61 35) . 1)
                      ((71 32) . 1) ((65 34) . 1) ((63 31) . 1)
                      ((63 37) . 1) ((75 34) . 1) ((51 33) . 1)
                      ((75 30) . 1) ((67 35) . 1) ((52 33) . 1)
                      ((73 34) . 1) ((86 31) . 1) ((71 33) . 1)
                      ((61 34) . 1) ((68 34) . 1) ((71 31) . 1)
                      ((72 31) . 1) ((73 30) . 1) ((64 31) . 1)
                      ((64 37) . 1) ((85 32) . 1) ((75 29) . 1)
                      ((75 35) . 1) ((52 34) . 1) ((66 36) . 1)
                      ((62 36) . 1) ((51 34) . 1) ((72 33) . 1)))

