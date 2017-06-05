#lang racket
(require racket/gui)
(require racket/draw)
(require profile)

; Constants
(define INIT-FRAME-HEIGHT 700)
(define INIT-FRAME-WIDTH 1000)
(define INIT-CELL-LENGTH 5)
(define INIT-SLEEP-DELAY (/ 1 10000))
(define color-black (make-object color% 0 0 0))
(define color-dead-str "black")
(define color-alive-str "green")
(define sim-started 'false)
(define ALIVE 1)
(define DEAD 0)

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

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STATE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Copys the keys in a list (and their value) from ht1 to ht2
(define (copy-ht keys ht1 ht2)
  (for ([key keys])
    (hash-set! ht2 key (hash-ref ht1 key))))

; Populates the hash table with dead cells
(define (cell-init-table ht max-x max-y)
  (for ([i (in-range 0 (add1 max-x))])
    (for ([j (in-range 0 (add1 max-y))])
      (let ((key (list i j)))
        (hash-set! ht key DEAD)))))

; Increases/decreases the size of the table
; Used to dynamically adjust the table size when resizing the frame
(define (cell-resize-table ht curr-x curr-y)
  (let ((new-max-x (exact-round (/ frame-width cell-length)))
        (new-max-y (exact-round (/ frame-height cell-length))))
    (cond ((and (> new-max-x curr-x) (> new-max-y curr-y)) ; height AND width increased
           (for ([i (in-range curr-x (add1 new-max-x))])
             (for ([j (in-range 0 (add1 new-max-y))])
               (hash-set! ht (list i j) DEAD)))
           (for ([i (in-range 0 (add1 new-max-x))])
             (for ([j (in-range curr-y (add1 new-max-y))]) 
               (hash-set! ht (list i j) DEAD))))
          ((> new-max-x curr-x)                            ; width increased only
           (for ([i (in-range curr-x (add1 new-max-x))])
             (for ([j (in-range 0 (add1 curr-y))])
               (hash-set! ht (list i j) DEAD))))
          ((> new-max-y curr-y)                            ; height increased only
           (for ([i (in-range 0 (add1 curr-x))])
             (for ([j (in-range curr-y (add1 new-max-y))])
               (hash-set! ht (list i j) DEAD))))
          ((or (< new-max-x curr-x) (< new-max-y curr-y))  ; height OR width decreased
           (display 'shit)
           (let ((new-hash (make-hash)))
           (for ([i (in-range 0 (add1 new-max-x))])
             (for ([j (in-range 0 (add1 new-max-y))])
               (hash-set! new-hash (list i j) (hash-ref cell-ht (list i j)))))
             (set! cell-ht (hash-copy new-hash)))))
    (set! max-x new-max-x)
    (set! max-y new-max-y)))

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

; Get a list of all keys for all alive cells and their neighbors
(define (cell-active ht)
  (for*/list ([(key value) (in-hash ht)]
             [i (cell-neighbors-moore key)]
             #:when (equal? value ALIVE))
    key i))

; Updates the state of a cell based on its neighbors
(define (cell-next-gen key status num-alive ht-buf)
    (cond ((equal? status ALIVE)
           (cond ((or (< num-alive 2) (> num-alive 3))
                  (hash-set! ht-buf key DEAD))  
                 ((> num-alive 3)
                  (hash-set! ht-buf key DEAD)))) 
          (else
           (equal? status DEAD)
           (cond ((equal? num-alive 3)
                  (hash-set! ht-buf key ALIVE))))))

; Calculates the next generation of the board
(define (board-next-gen active-cells ht ht-buf)
  (let ((alive-count 0))
  (for ([key active-cells])
    (let ((value (hash-ref ht key))
          (neighbors (cell-neighbors-moore key)))
      (for ([neighbor-key neighbors])
        (let ((neighbor-value (hash-ref ht neighbor-key)))
          (set! alive-count (+ alive-count neighbor-value))))
      (cell-next-gen key value alive-count ht-buf)
      (set! alive-count 0)))))

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
           (draw-square key color-alive-str))
          ((equal? status DEAD)
           (draw-square key color-dead-str)))))

; Toggles a cell's status and draws it on the canvas (used for mouse actions)
(define (cell-toggle-status ht key)
  (let ((status (hash-ref ht key)))
    (cond ((equal? status ALIVE)
           (hash-set! ht key DEAD)
           (draw-square key color-dead-str))
          ((equal? status DEAD)
           (hash-set! ht key ALIVE)
           (draw-square key color-alive-str)))))

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
           (cond ((equal? key-char 'f1) (start-loop))))) ; start the simulation with f1
    (define/override (on-paint)                          ; on-paint override
      (begin
        (send board-canvas set-canvas-background color-black)
        (let ((curr-frame-height (send frame get-height))
              (curr-frame-width (send frame get-width)))
        (cond ((and (equal? sim-started 'true)
               (not (and (equal? curr-frame-height INIT-FRAME-HEIGHT)  ; handles window resizing
                         (equal? curr-frame-width INIT-FRAME-WIDTH))))
               (set! frame-height curr-frame-height)
               (set! frame-width curr-frame-width)
                   (cell-resize-table cell-ht max-x max-y)
                   (draw-board cell-ht))))))
    (super-new)))

(define frame (new frame%
                   [label "Game of Life"]
                   [width frame-width]
                   [height frame-height]))

(define board-canvas (new game-canvas%	 
                    [parent frame]	 
                    [style '()]))

(define dc (send board-canvas get-dc))

; Draws a single square on the canvas at (x,y)
(define (draw-square key color)
  (let ((x (car key))
        (y (cadr key)))
  (begin
    (send dc set-brush (make-object brush% color 'solid))
    (send dc set-pen (make-object pen% color-black 0 'solid))
    (send dc draw-rectangle
          (* x cell-length)
          (* y cell-length)
          cell-length cell-length))))

; Draws the cells in ht on the board
(define (draw-board ht)
  (for ([(key status) (in-hash ht)])
      (cond ((equal? status ALIVE) (draw-square key color-alive-str))
            ((equal? status DEAD) (draw-square key color-dead-str)))))


(send frame show #t)
(sleep/yield 0)

; Initialization
(cell-init-table cell-ht max-x max-y)     
(draw-board cell-ht)                 
(set! sim-started 'true)

; Produces one iteration of the game
(define (game-one-iter)
  (begin
    (let ((active-cells (cell-active cell-ht)))
      (copy-ht active-cells cell-ht cell-buf)
      (board-next-gen active-cells cell-ht cell-buf) 
      (draw-board cell-buf)     
      (copy-ht active-cells cell-buf cell-ht))))

; Main loop
(define (start-loop)
  (for ([i (in-range 0 300)])
  (begin
    (sleep/yield sleep-delay)
    (game-one-iter))))




