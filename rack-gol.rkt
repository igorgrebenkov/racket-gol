#lang racket
(require racket/gui)
(require racket/draw)

; Constants
(define INIT-FRAME-HEIGHT 1000)
(define INIT-FRAME-WIDTH 700)
(define cell-length 5)           
(define color-black (make-object color% 0 0 0))
(define color-dead-str "black")
(define color-alive-str "green")
(define sleep-delay (/ 1 10))

; State variables
(define frame-height INIT-FRAME-HEIGHT)      
(define frame-width INIT-FRAME-WIDTH)
(define max-x
  (exact-round (/ frame-width cell-length)))
(define max-y
  (exact-round (/ frame-height cell-length)))
(define cell-keys '())
(define cell-ht (make-hash))
(define cell-buf (make-hash))

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STATE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; Generates a list of all possible cell keys
(define (gen-xy max-x max-y)
  (begin
    (do ((i max-x (sub1 i))) ((< i 0))
      (do ((j max-y (sub1 j))) ((< j 0))
           (set! cell-keys
                 (cons (list i j) cell-keys))))))

; Populates the hash table with every cell initially dead
(define (init-table ht key-list)
  (cond ((null? key-list) #t)
        (else
         (let ((key (car key-list)))
           (begin
             (hash-set! ht key 'dead)
             (init-table ht (cdr key-list)))))))

(define (update-table ht)
  (for ([(key status) (in-hash ht)])
    (cond ((not (hash-has-key? ht key))
           (hash-set! ht key 'dead)))))

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
(define (board-next-gen ht ht-buf key-list)
  (cond ((null? key-list) #t)
        (else
         (cell-next-gen ht ht-buf (car key-list))
         (board-next-gen ht ht-buf (cdr key-list)))))

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
    (define/override (on-paint)
      (begin
        (send board-canvas set-canvas-background color-black)
        (cond ((not (and (equal? frame-height INIT-FRAME-HEIGHT)  ; handles window resizing
                        (equal? frame-height INIT-FRAME-WIDTH)))
               (set! frame-height (send frame get-height))
               (set! frame-width (send frame get-width))
               (set! max-x (exact-round (/ frame-width cell-length)))
               (set! max-y (exact-round (/ frame-height cell-length)))
               (set! cell-keys '())
               (gen-xy max-x max-y)
               (update-table cell-ht)
               (draw-board cell-ht)))))
      (super-new)))

(define frame (new frame%
                   [label "Game of Life"]
                   [width frame-height]
                   [height frame-width]))

(define board-canvas (new game-canvas%	 
                    [parent frame]	 
                    [style '()]
                    [paint-callback
                     (lambda (canvas dc)
                       (send canvas set-canvas-background color-black))]))

(define dc (send board-canvas get-dc))

; Draws a single square on the canvas at (x,y)
(define (draw-square key color)
  (let ((x (car key))
        (y (cadr key)))
  (begin
    (send dc set-brush (make-object brush% color 'solid))
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

; Initialize hash table with dead cells and draw it
(gen-xy max-x max-y)
(init-table cell-ht cell-keys)
(set! cell-buf (hash-copy cell-ht))
(draw-board cell-ht)

; Produces one iteration of the game
(define (game-one-iter)
  (begin
    (set! cell-buf (hash-copy cell-ht))         ; copy current state to buffer
    (board-next-gen cell-ht cell-buf cell-keys) ; get next state in buffer
    (draw-board                                 ; draw cells that have changed state
     (hash-diff cell-ht cell-buf))     
    (set! cell-ht (hash-copy cell-buf))))       ; buffer becomes new current state 

; Main loop
(define (start-loop)
  (begin
    (game-one-iter)
    ;(sleep/yield 0)
    (start-loop)))




