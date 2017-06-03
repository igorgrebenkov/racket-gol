#lang racket
(require racket/gui)
(require racket/draw)

(define cell-length 20)           
(define frame-height 1000)      
(define frame-width 1000)
(define color-black (make-object color% 0 0 0))
(define color-dead-str "black")
(define color-alive-str "green")
(define sleep-delay 1)
(define max-x
  (exact-round (/ frame-width cell-length)))
(define max-y
  (exact-round (/ frame-height cell-length)))
(define cell-keys '())

; Generates a list of all possible cell keys
(define (gen-xy max-x max-y)
  (begin
    (do ((i max-y (sub1 i))) ((< i 0))
      (do ((j max-x (sub1 j))) ((< j 0))
           (set! cell-keys
                 (cons (list i j) cell-keys))))
    ))

; Draws a single square on the canvas at (x,y)
(define (draw-square x y color)
  (begin
    (send dc set-brush (make-object brush% color 'solid))
    (send dc draw-rectangle
          (* x cell-length)
          (* y cell-length)
          cell-length cell-length)))

; Hash table for the cells
(define cell-ht (make-hash))

; Sets a cell's status and draws it on the canvas
(define (cell-update-status ht key status)
  (begin
    (hash-set! ht key status)
    (cond ((equal? status 'alive)
           (draw-square (car key) (cadr key) color-alive-str))
          ((equal? status 'dead)
           (draw-square (car key) (cadr key) color-dead-str)))))

; Toggles a cell's status and draws it on the canvas
(define (cell-toggle-status ht key)
  (let ((status (hash-ref ht key)))
    (cond ((equal? status 'alive)
           (hash-set! ht key 'dead)
           (draw-square (car key) (cadr key) color-dead-str))
          ((equal? status 'dead)
           (hash-set! ht key 'alive)
           (draw-square (car key) (cadr key) color-alive-str)))))

; Populates the hash table with every cell initially dead
(define (populate-table ht key-list)
  (cond ((null? key-list) #t)
        (else
         (let ((key (car key-list)))
           (begin
             (hash-set! ht key 'dead)
             (populate-table ht (cdr key-list)))))))
  
; Draws the cells in the coord-list to the board-canvas
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


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
        (set! frame-height (send frame get-height))
        (set! frame-width (send frame get-width))
        (set! max-x (exact-round (/ frame-height cell-length)))
        (set! max-y (exact-round (/ frame-width cell-length)))
        (set! cell-keys '())
        (gen-xy max-x max-y)
        (populate-table cell-ht cell-keys)
        (draw-cells cell-ht cell-keys)))
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

(send frame show #t)
(sleep/yield sleep-delay)

(gen-xy max-x max-y)
(populate-table cell-ht cell-keys)
(draw-cells cell-ht cell-keys)




