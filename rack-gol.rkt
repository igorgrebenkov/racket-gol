#lang racket
(require racket/gui)
(require racket/draw)
(require math/array)

(define frame-height 1000)
(define frame-width 1000)
(define color-black (make-object color% 0 0 0))

(define frame (new frame%
                   [label "Game of Life"]
                   [width frame-height]
                   [height frame-width]))

(define canvas (new canvas%	 
                    [parent frame]	 
                    [style '()]
                    [paint-callback
                     (lambda (canvas dc)
                       (send canvas set-canvas-background (make-object color% color-black))
                       (send dc set-brush "green" 'solid))])) ; 30 pixels wide, and 10 pixels high

(define dc (send canvas get-dc))

(send frame show #t)
(sleep/yield 1)

; The length of the sides of each cell
(define c-length 20)

; Finds the max value for the x/y co-ordinates
(define max-xy
  (exact-round (/ frame-height c-length)))

; Draws a single square on the canvas at (x,y)
(define (draw-square x y length)
  (send dc draw-rectangle
        (* x length)
        (* y length)
        length length))

; List of all the cell keys in the hash table
(define cell-keys '())

; Hash table for the cells
(define cell-ht (make-hash))

; Inserts a cell into the hash table with key '(x y)
(define (insert-cell ht x y status)
  (hash-set! ht (list x y) (list status)))


; Generates a list of all possible cell keys
(define (gen-xy max)
  (begin
    (do ((i max (sub1 i))) ((< i 0))
      (do ((j max (sub1 j))) ((< j 0))
           (set! cell-keys
                 (cons (list i j) cell-keys))))
    cell-keys))


(define (populate-table ht key-list)
  (cond ((null? key-list) #t)
        (else
         (let ((key (car key-list)))
           (begin
             (hash-set! ht key 'dead)
             (populate-table ht (cdr key-list)))))))
           



; Draws the cells in the coord-list to the canvas
(define (draw-cells ht key-list)
  (cond ((null? key-list) #t)
        (else
         (let* ((key (car key-list))
               (status (hash-ref ht key))
               (x (car key))
               (y (cadr key)))
          (cond ((equal? status 'dead)
                 (draw-square x y c-length)
                 (draw-cells ht (cdr key-list)))
                (else
                 (draw-cells ht (cdr key-list))))))))


         
         
;(populate-table cell-ht (gen-xy max-xy))




