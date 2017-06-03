#lang racket
(require racket/gui)
(require racket/draw)
(require math/array)

(define frame-height 1000)
(define frame-width 1000)
(define color-black (make-object color% 0 0 0))

(define frame (new frame%
                   [label "Sierpinski Triangle Generator"]
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

; Represents a single cell
(define (cell x y status) (list x y status))

; List of all the cells on the board
(define cells '())

; Generates a list of all cells
(define (gen-xy max)
  (begin
    (do ((i (add1 max) (sub1 i))) ((< i 0))
      (do ((j (add1 max) (sub1 j))) ((< j 0))
           (set! cells (cons (cell i j 'alive) cells))))
    cells))

; Draws the cells in the coord-list to the canvas
(define (draw-cells coord-list)
  (cond ((null? coord-list) #t)
        (else
         (let ((x (caar coord-list))
               (y (cadar coord-list))
               (status (caddar coord-list)))
          (cond ((equal? status 'alive)
                 (draw-square x y c-length)
                 (draw-cells (cdr coord-list)))
                (else
                 (draw-cells (cdr coord-list))))))))

         





