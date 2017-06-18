#lang racket
(require racket/gui)
(require racket/draw)
(require profile)

; Constants
(define INIT-FRAME-HEIGHT 580)
(define INIT-FRAME-WIDTH 750)
(define INIT-CELL-LENGTH 5)
(define INIT-SLEEP-DELAY (/ 1 10))
(define ALIVE 1)
(define DEAD 0)
(define CONWAY-ALIVE-UPPER 3)
(define CONWAY-ALIVE-LOWER 2)
(define CONWAY-DEAD-THRESH 3)

(define color-background (make-object color% 0 0 0))
(define color-dead "black")
(define color-alive "green")
(define cell-inner-style 'solid)
(define cell-border-style 'transparent)
(define cell-alive-brush (make-object brush% color-alive cell-inner-style))
(define cell-dead-brush (make-object brush% color-dead cell-inner-style))

; State variables
(define cell-length INIT-CELL-LENGTH)  
(define frame-height INIT-FRAME-HEIGHT)      
(define frame-width INIT-FRAME-WIDTH)
(define sleep-delay INIT-SLEEP-DELAY)
(define max-x
  (exact-round (/ frame-width cell-length)))
(define max-y
  (exact-round (/ frame-height cell-length)))

(define cell-keys '())
(define cell-ht (make-hash))
(define cell-buf (make-hash))
(define cell-neighbor (make-hash))
(define alive-upper-lim CONWAY-ALIVE-UPPER)
(define alive-lower-lim CONWAY-ALIVE-LOWER)
(define dead-thresh CONWAY-DEAD-THRESH)

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STATE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; Returns a hash table of elements in ht2 with different
; values for elements of the same key in ht1
(define (hash-diff ht1 ht2)
  (for/hash ([(key value) (in-hash ht2)]
             #:when (not (equal? value (hash-ref ht1 key))))
    (values key value)))

; Initializes the cell hash tables
;
; Initially, cell-ht only contains alive cells selected by the user.
; The keys list provided to the function holds the keys of all alive cells
; and their neighbors. Hence, if any of those neighbors are dead,
; they won't be in cell-ht yet, and so we add them.
;
; Otherwise, if cell-ht does contain an element with that key, it
; must be a live cell, and so we copy it to cell-buf. This has to
; be done so that cells that don't change state won't be lost when
; assigning cell-buf back to cell-ht at the end of each generation.
(define (init-ht2 keys ht1 ht2)
  (for ([key keys])
    (cond ((hash-has-key? ht1 key)
           (hash-set! ht2 key (hash-ref ht1 key)))
          (else
           (hash-set! ht1 key DEAD)))))

(define (init-ht ht0 ht1 ht2)
  (for ([(key value) (in-hash ht0)])
    (cond ((hash-has-key? ht1 key)
           (hash-set! ht2 key (hash-ref ht1 key)))
          (else
           (hash-set! ht1 key DEAD)))))

; Generates a list of the Moore neighborhood of a cell
(define (cell-neighbors-moore key)
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

; Get a list of keys for neighbors of alive cells
(define (cell-active2 ht)
  (let ((result '()))
    (for ([(key value) (in-hash ht)]
           #:when (equal? value ALIVE))
      (cond ((hash-has-key? cell-neighbor key)
             (set! result (append (hash-ref cell-neighbor key) result)))
            (else
             (let ((neighbors (cell-neighbors-moore key)))
               (hash-set! cell-neighbor key neighbors)
               (set! result (append neighbors result))))))
            result))

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

; Calculates the next generation of the board
(define (board-next-gen2 ht ht-buf)
  (let ((alive-count 0))
  (for ([(key value) (in-hash ht)])
    (let ((neighbors (cond ((hash-has-key? cell-neighbor key)
                            (hash-ref cell-neighbor key))
                           (else
                            (cell-neighbors-moore key)))))
      (for ([neighbor-key neighbors])
        (cond ((hash-has-key? ht neighbor-key)
               (let ((neighbor-value (hash-ref ht neighbor-key)))
                 (set! alive-count (+ alive-count neighbor-value))))))
      (cell-next-gen key value alive-count ht-buf)
      (set! alive-count 0)))))

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
           (alive-count (cell-neighbor-count ht key neighbors)))
      (cell-next-gen key value alive-count ht-buf)
      (set! alive-count 0))))

(define (cell-seed2 ht)
  (for ([i (in-range 0 max-x)])
    (for ([j (in-range 0 max-y)])
    (cond ((equal? (random 12) 0)
           (hash-set! ht (list i j) ALIVE))))
  (draw-board ht)))

(define (cell-seed ht)
  (for ([i (in-range 0 max-x)])
    (hash-set! ht (list i 80) ALIVE))
  (draw-board ht))

(define gosper '#hash(((67 34) . 1)
       ((67 33) . 1)
       ((62 32) . 1)
       ((86 32) . 1)
       ((66 32) . 1)
       ((72 32) . 1)
       ((85 31) . 1)
       ((61 33) . 1)
       ((61 35) . 1)
       ((71 32) . 1)
       ((65 34) . 1)
       ((63 31) . 1)
       ((63 37) . 1)
       ((75 34) . 1)
       ((51 33) . 1)
       ((75 30) . 1)
       ((67 35) . 1)
       ((52 33) . 1)
       ((73 34) . 1)
       ((86 31) . 1)
       ((71 33) . 1)
       ((61 34) . 1)
       ((68 34) . 1)
       ((71 31) . 1)
       ((72 31) . 1)
       ((73 30) . 1)
       ((64 31) . 1)
       ((64 37) . 1)
       ((85 32) . 1)
       ((75 29) . 1)
       ((75 35) . 1)
       ((52 34) . 1)
       ((66 36) . 1)
       ((62 36) . 1)
       ((51 34) . 1)
       ((72 33) . 1)))

(define (draw-gosper)
  (begin
    (for ([(key value) (in-hash gosper)])
      (hash-set! cell-ht key value))
    (draw-board cell-ht)))
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% VIEW %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; Action associated with mouse buttons for making cells dead/alive
(define (mouse-click-action x y action)
  (let ((cell-x (exact-floor (/ x cell-length)))
        (cell-y (exact-floor (/ y cell-length))))
    (cond ((equal? action 'toggle-life)
           (cell-toggle-status cell-ht (list cell-x cell-y)))
          ((equal? action 'give-life)
           (cell-update-status cell-ht (list cell-x cell-y) ALIVE))
          ((equal? action 'kill)
           (cell-update-status cell-ht (list cell-x cell-y) DEAD)))))

; Sets a cell's status and draws it on the canvas (used for mouse actions)
(define (cell-update-status ht key status)
  (begin
    (hash-set! ht key status)
    (cond ((equal? status ALIVE)
           (draw-square key cell-alive-brush))
          ((equal? status DEAD)
           (draw-square key cell-dead-brush)))))

; Toggles a cell's status and draws it on the canvas (used for mouse actions)
(define (cell-toggle-status ht key)
  (cond ((hash-has-key? ht key)
         (let ((status (hash-ref ht key)))
           (cond ((equal? status ALIVE)
                  (hash-set! ht key DEAD)
                  (draw-square key cell-dead-brush))
                 ((equal? status DEAD)
                  (hash-set! ht key ALIVE)
                  (draw-square key cell-alive-brush)))))
        (else
         (hash-set! ht key ALIVE)
         (draw-square key cell-alive-brush))))

; Custom canvas used for the gameboard
(define game-canvas%
  (class canvas%
    ; Left click         -> toggle cell status
    ; Left click + drag  -> make dead cells alive
    ; Right click + drag -> make alive cells dead
    (define/override (on-event event)
      (cond ((send event button-down? 'left)  ; left click
               (mouse-click-action
                (send event get-x)
                (send event get-y)
                'toggle-life))
            ((and (send event get-left-down)  ; left click + drag
                  (send event dragging?))
             (mouse-click-action
              (send event get-x)
              (send event get-y)
              'give-life))
            ((and (send event get-right-down) ; right click + drag
                  (send event dragging?))
             (mouse-click-action
              (send event get-x)
              (send event get-y)
              'kill))
            (else #f)))
    (define/override (on-char event)                     ; Keyboard events
      (let ((key-char (send event get-key-code)))
           (cond ((equal? key-char 'f1) (begin (start-loop) (display 'test)))
                 ((equal? key-char 'f2) (draw-gosper))
                 ((equal? key-char 'f3)
                  (send board-canvas refresh)
                  (set! cell-length (+ cell-length 0.3))
                  (set! max-x
                    (exact-round (/ frame-width cell-length)))
                  (set! max-y
                    (exact-round (/ frame-height cell-length))))
                 ((equal? key-char 'f4)
                  (send board-canvas refresh)
                  (set! cell-length (- cell-length 0.3))
                  (set! max-x
                    (exact-round (/ frame-width cell-length)))
                  (set! max-y
                    (exact-round (/ frame-height cell-length))))))) 
    (define/override (on-paint)                         
      (begin
        ; disabling this line looks cool
        (send board-canvas set-canvas-background color-background)
        (draw-board cell-ht)
        (let ((curr-frame-height (send frame get-height))
              (curr-frame-width (send frame get-width)))
        (cond ((not (and (equal? curr-frame-height INIT-FRAME-HEIGHT)  ; handles window resizing
                         (equal? curr-frame-width INIT-FRAME-WIDTH)))
               (set! frame-height curr-frame-height)
               (set! frame-width curr-frame-width)
               (set! max-x (exact-round (/ frame-width cell-length)))
               (set! max-y (exact-round (/ frame-height cell-length))))))))
    (super-new)))

(define frame (new frame%
                   [label "Game of Life"]
                   [width frame-width]
                   [height frame-height]))

(define board-canvas (new game-canvas%	 
                          [parent frame]	 
                          [style '()]))

(define dc (send board-canvas get-dc))

; Draws a single square on the canvas at '(x,y)
(define (draw-square key brush)
  (let ((x (car key))
        (y (cadr key)))
    (send dc set-brush brush)
    (send dc draw-rectangle
          (* x cell-length)
          (* y cell-length)
          cell-length cell-length)))

; Draws the cells in ht on the board
(define (draw-board ht)
  (for ([(key status) (in-hash ht)])
      (cond ((equal? status ALIVE) (draw-square key cell-alive-brush))
            ((equal? status DEAD) (draw-square key cell-dead-brush)))))

; Initialization
(send frame show #t)
(sleep/yield 0)
(send board-canvas refresh)
(send board-canvas focus)
(send dc set-pen (make-object pen% color-dead 1 cell-border-style))
(collect-garbage)

(define (cell-remove-dead ht)
  (for/hash ([(key value) (in-hash ht)]
             #:when (equal? value ALIVE))
    (values key value)))
   


; Produces one iteration of the game
(define (game-one-iter)
  (begin
    (init-ht (cell-active cell-ht) cell-ht cell-buf)
    (board-next-gen cell-ht cell-buf)
    (draw-board (hash-diff cell-ht cell-buf))
    (set! cell-ht (hash-copy (cell-remove-dead cell-buf)))
    (set! cell-buf (make-hash))
    (collect-garbage 'incremental)))
   
; Main loop
(define (start-loop)
    (for ([i (in-range 0 +inf.0)])
      (game-one-iter)
      (sleep/yield sleep-delay)))





