#lang racket
(require racket/gui)
(require racket/draw)
(require math/array)

; The length of the sides of each cell
(define c-length 20)
(define frame-height 1000)
(define frame-width 1000)
(define color-black (make-object color% 0 0 0))
(define color-red-str "red")
(define color-green-str "green")

(define (mouse-click-action x y)
  (let ((cell-x (exact-floor (/ x c-length)))
        (cell-y (exact-floor (/ y c-length))))
    (cell-toggle-status cell-ht (list cell-x cell-y))))

(define game-canvas%
  (class canvas%
    (define/override (on-event event)
      (cond ((or (send event button-down? 'left)
                 (send event get-left-down))
             (let ((x (send event get-x))
                   (y (send event get-y)))
               (mouse-click-action x y)))
            (else #f)))
      (super-new)))
  
(define frame (new frame%
                   [label "Game of Life"]
                   [width frame-height]
                   [height frame-width]))

(define canvas (new game-canvas%	 
                    [parent frame]	 
                    [style '()]
                    [paint-callback
                     (lambda (canvas dc)
                       (send canvas set-canvas-background color-black))]))

(define dc (send canvas get-dc))

(send frame show #t)
(sleep/yield 1)

; Finds the max value for the x/y co-ordinates
(define max-xy
  (exact-round (/ frame-height c-length)))

; Draws a single square on the canvas at (x,y)
(define (draw-square x y length color)
  (begin
    (send dc set-brush (make-object brush% color 'solid))
    (send dc draw-rectangle
          (* x length)
          (* y length)
          length length)))



; Hash table for the cells
(define cell-ht (make-hash))

; Sets a cell's status and draws it on the canvas
(define (cell-update-status ht key status)
  (begin
    (hash-set! ht key status)
    (cond ((equal? status 'alive)
           (draw-square (car key) (cadr key) c-length color-green-str))
          ((equal? status 'dead)
           (draw-square (car key) (cadr key) c-length color-red-str)))))

(define (cell-toggle-status ht key)
  (let ((status (hash-ref ht key)))
    (cond ((equal? status 'alive)
           (hash-set! ht key 'dead)
           (draw-square (car key) (cadr key) c-length color-red-str))
          ((equal? status 'dead)
           (hash-set! ht key 'alive)
           (draw-square (car key) (cadr key) c-length color-green-str)))))

; List of all the cell keys in the hash table
(define cell-keys '())

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


(define (populate-random-alive ht key-list)
  (cond ((null? key-list) #t)
        (else
         (let ((key (car key-list))
               (rand (random 1 10 (make-pseudo-random-generator))))
           (begin
             (cond ((or (equal? rand 2) (equal? rand 5) (equal? rand 7) (equal? rand 9))
                    (hash-set! ht key 'alive)
                    (populate-random-alive ht (cdr key-list)))
                   (else
                    (populate-random-alive ht (cdr key-list)))))))))          


(define (init-alive ht list)
  (cond ((null? list) #t)
        (else
         (hash-set! ht (car list) 'alive)
         (init-alive ht (cdr list)))))


; Draws the cells in the coord-list to the canvas
(define (draw-cells ht key-list)
  (cond ((null? key-list) #t)
        (else
         (let* ((key (car key-list))
               (status (hash-ref ht key))
               (x (car key))
               (y (cadr key)))
          (cond ((equal? status 'dead)
                 (cell-update-status ht key 'dead) 
                 (draw-cells ht (cdr key-list)))
                ((equal? status 'alive)
                 (cell-update-status ht key 'alive) 
                 (draw-cells ht (cdr key-list)))
                (else
                 (draw-cells ht (cdr key-list))))))))




(populate-table cell-ht (gen-xy max-xy))
(populate-random-alive cell-ht (gen-xy max-xy))
(draw-cells cell-ht (gen-xy max-xy))




