#lang racket
(require racket/gui)
(require racket/draw)

; Constants
(define INIT-FRAME-HEIGHT 700)
(define INIT-FRAME-WIDTH 1000)
(define INIT-CELL-LENGTH 20)
(define INIT-SLEEP-DELAY (/ 1 25))
(define color-black (make-object color% 0 0 0))
(define color-dead-str "black")
(define color-alive-str "green")
(define sim-started 'false)

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

; Populates the hash table with dead cells
(define (cell-init-table ht max-x max-y)
  (for ([i (in-range 0 (add1 max-x))])
    (for ([j (in-range 0 (add1 max-y))])
      (let ((key (list i j)))
        (hash-set! ht key 'dead)))))

; Increases the size of the table
; Used to dynamically adjust the table size when resizing the frame
(define (cell-resize-table ht curr-x curr-y)
  (let ((new-max-x (exact-round (/ frame-width cell-length)))
        (new-max-y (exact-round (/ frame-height cell-length))))
    (cond ((and (> new-max-x curr-x) (> new-max-y curr-y)) ; height/width increased
           (for ([i (in-range curr-x (add1 new-max-x))])
             (for ([j (in-range 0 (add1 new-max-y))])
               (hash-set! ht (list i j) 'dead)))
           (for ([i (in-range 0 (add1 new-max-x))])
             (for ([j (in-range curr-y (add1 new-max-y))]) 
               (hash-set! ht (list i j) 'dead))))
          ((> new-max-x curr-x)                            ; width increased only
           (for ([i (in-range curr-x (add1 new-max-x))])
             (for ([j (in-range 0 (add1 curr-y))])
               (hash-set! ht (list i j) 'dead))))
          ((> new-max-y curr-y)                            ; height increased only
           (for ([i (in-range 0 (add1 curr-x))])
             (for ([j (in-range curr-y (add1 new-max-y))])
               (hash-set! ht (list i j) 'dead)))))
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

; Returns a list with the number of alive/dead cells -> '(alive dead)
(define (cell-neighbors-count ht key)
  (let ((num-alive 0)
        (num-dead 0)
        (neighbors (cell-neighbors-moore key)))
    (letrec ((count-loop (lambda (neighbor-list)
                           (cond ((null? neighbor-list) #t)
                                 (else
                                  (let* ((key (car neighbor-list))
                                         (status (hash-ref ht key)))
                                    (cond ((equal? status 'alive)
                                           (set! num-alive (add1 num-alive))
                                           (count-loop (cdr neighbor-list)))
                                          ((equal? status 'dead)
                                           (set! num-dead (add1 num-dead))
                                           (count-loop (cdr neighbor-list))))))))))
      (count-loop neighbors))
    (list num-alive num-dead)))

; Returns a hash table containing all elements in ht2
; whose value differs from that in ht1 with the same key
(define (hash-diff ht1 ht2)
  (let ((result-ht (make-hash)))
    (for ([(key value1) (in-hash ht1)])
      (let ((value2 (hash-ref ht2 key)))
        (cond ((not (equal? value1 value2))
               (hash-set! result-ht key value2)))))
    result-ht))
                                                           
; Updates the state of a cell based on its neighbors
(define (cell-next-gen ht ht-buf key)
  (let* ((neighbors (cell-neighbors-moore key))
         (num-neighbors (cell-neighbors-count ht key))
         (num-alive (car num-neighbors))
         (num-dead (cadr num-neighbors))
         (status (hash-ref ht key)))
    (cond ((equal? status 'alive)
           (cond ((< num-alive 2) (hash-set! ht-buf key 'dead))  
                 ((> num-alive 3) (hash-set! ht-buf key 'dead)))) 
          ((equal? status 'dead)
           (cond ((equal? num-alive 3) (hash-set! ht-buf key 'alive)))))))

; Updates the cell buffer with the next state of the board
(define (board-next-gen ht ht-buf)
  (for ([(key value1) (in-hash ht)])
    (cell-next-gen ht ht-buf key)))

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% VIEW %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; Action associated with mouse buttons for making cells dead/alive
(define (mouse-click-action x y action)
  (let ((cell-x (exact-floor (/ x cell-length)))
        (cell-y (exact-floor (/ y cell-length))))
    (cond ((equal? action 'toggle-life)
           (cell-toggle-status cell-ht (list cell-x cell-y)))
          ((equal? action 'give-life)
           (cell-update-status cell-ht (list cell-x cell-y) 'alive))
          ((equal? action 'kill)
           (cell-update-status cell-ht (list cell-x cell-y) 'dead)))))

; Sets a cell's status and draws it on the canvas (used for mouse actions)
(define (cell-update-status ht key status)
  (begin
    (hash-set! ht key status)
    (cond ((equal? status 'alive)
           (draw-square key color-alive-str))
          ((equal? status 'dead)
           (draw-square key color-dead-str)))))

; Toggles a cell's status and draws it on the canvas (used for mouse actions)
(define (cell-toggle-status ht key)
  (let ((status (hash-ref ht key)))
    (cond ((equal? status 'alive)
           (hash-set! ht key 'dead)
           (draw-square key color-dead-str))
          ((equal? status 'dead)
           (hash-set! ht key 'alive)
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
      (cond ((equal? status 'alive) (draw-square key color-alive-str))
            ((equal? status 'dead) (draw-square key color-dead-str)))))

(send frame show #t)
(sleep/yield 0)

; Initialization
(cell-init-table cell-ht max-x max-y)     
(set! cell-buf (hash-copy cell-ht))  
(draw-board cell-ht)                 
(set! sim-started 'true)

; Produces one iteration of the game
(define (game-one-iter)
  (begin
    (set! cell-buf (hash-copy cell-ht))         ; copy current state to buffer
    (board-next-gen cell-ht cell-buf) ; get next state in buffer
    (let ((hash-changes (hash-diff cell-ht cell-buf)))
    (draw-board                                 ; draw cells that have changed state
     (hash-diff cell-ht cell-buf))     
    (set! cell-ht cell-buf))))      ; buffer becomes new current state 

; Main loop
(define (start-loop)
  (begin
    (set! sim-started 'true)
    (game-one-iter)
    (sleep/yield sleep-delay)
    (start-loop)))




