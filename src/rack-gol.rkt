#lang racket

; Contains the game view, I/O and Main loop

(require racket/gui)
(require racket/draw)
(require "global-vars.rkt")
(require "cells.rkt")

; ************************* GAME BOARD CANVAS DEFINITION **************************
(define game-canvas%
  (class canvas%
    ; Left click         -> toggle cell status
    ; Left click + drag  -> make dead cells alive
    ; Right click + drag -> make alive cells dead
    (define/override (on-event event)
      (begin
        (set-mouse-x! (send event get-x))       ; capture current mouse xy for zooming
        (set-mouse-y! (send event get-y))
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
              (else #f))))
    (define/override (on-char event)          ; mouse wheel zoom
      (cond ((equal? 'wheel-up (send event get-key-code))
             (mouse-board-zoom 'up event))
            ((equal? 'wheel-down (send event get-key-code))
             (mouse-board-zoom 'down event))))
    (define/override (on-paint)                         
      (begin
        (send board-canvas set-canvas-background color-background)
        (draw-board cell-ht)
         ; handles window resizing
        (let-values ([(curr-board-width curr-board-height)   
                      (send board-canvas get-client-size)])
          (cond ((or (< curr-board-height board-height)  
                     (< curr-board-width board-width))
                 (update-max-xy)
                 (set-cell-ht! (cell-trim cell-ht)))
                ((or (> curr-board-height board-height)
                     (> curr-board-width board-width))
                 (update-max-xy))))))
    (super-new)))

; ********************************* MOUSE-ACTIONS *********************************
; Zooms in on the board by increasing/decreasing cell length
(define (mouse-board-zoom direction event)
  (let* ((new-cell-length
          (cond ((equal? direction 'up)
                 (cond ((< cell-length MAX-CELL-LENGTH)
                        (add1 cell-length))
                       (else
                        MAX-CELL-LENGTH)))
                 ((equal? direction 'down)
                  (cond ((> cell-length MIN-CELL-LENGTH)
                         (sub1 cell-length))
                        (else
                         MIN-CELL-LENGTH)))))
         (curr-cell-length cell-length)
         (dx (round (/ (- (/ board-width new-cell-length)
                           (/ board-width curr-cell-length))
                       (/ board-width mouse-x))))
         (dy (round (/ (- (/ board-height new-cell-length)
                           (/ board-height curr-cell-length))
                       (/ board-height mouse-y)))))
    (set-cell-ht! (cell-offset cell-ht dx dy))
    (set-cell-length! new-cell-length)
    (send slider-cell-size set-value cell-length)
    (update-max-xy)
    (cond ((and (< cell-length 4)
                (equal? (send (send dc get-pen) get-style) 'solid))
           (change-cell-border 'transparent)
           (send checkbox-border set-value #f)))
    (send board-canvas refresh)))


; Updates cell hash table in response to mouse clicks on the board
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
    (cond ((equal? status ALIVE)
           (hash-set! ht key status)
           (draw-square key cell-alive-brush))
          ((equal? status DEAD)
           (hash-remove! ht key)
           (draw-square key cell-dead-brush))))

; Toggles a cell's status and draws it on the canvas (used for mouse actions)
(define (cell-toggle-status ht key)
  (cond ((hash-has-key? ht key)
         (let ((status (hash-ref ht key)))
           (cond ((equal? status ALIVE)
                  (hash-remove! ht key)
                  (draw-square key cell-dead-brush))
                 ((equal? status DEAD)
                  (hash-set! ht key ALIVE)
                  (draw-square key cell-alive-brush)))))
        (else
         (hash-set! ht key ALIVE)
         (draw-square key cell-alive-brush))))

; ********************************** MAIN FRAME ***********************************
(define main-frame (new frame% [label "Game of Life"]
                               [height INIT-FRAME-HEIGHT]
                               [width INIT-FRAME-WIDTH]))

; ******************************* GAME BOARD CANVAS *******************************
(define board-canvas (new game-canvas%	 
                          [parent main-frame]	 
                          [style '()]))
(define dc (send board-canvas get-dc))

; Updates the max-x and max-y global variables
; Used to trim the hash table when making the window smaller
(define (update-max-xy)
  (let-values ([(x y) (send board-canvas get-client-size)])
    (set-board-height! y)
    (set-board-width! x)
    (set-max-x! (exact-round (/ board-width 1)))
    (set-max-y! (exact-round (/ board-height 1)))))

; ******************************* TOP CONTROL PANEL *******************************
(define control-panel-top (new horizontal-panel%
                               [parent main-frame]
                               [alignment '(center center)]
                               [stretchable-height #f]
                               [stretchable-width #f]))

; Control panel GUI elements (top)
(define button-start
  (new button% [parent control-panel-top]
               [label "Start"]
               [callback (lambda (i e)
                           (set-sim-running! #t)
                           (game-loop))]))

(define button-stop
  (new button% [parent control-panel-top]
               [label "Stop"]
               [callback (lambda (i e)
                           (set-sim-running! #f))]))

(define button-next
  (new button% [parent control-panel-top]
               [label "Next"]
               [callback (lambda (i e)
                           (game-one-iter))]))

(define button-clear
 (new button% [parent control-panel-top]
              [label "Clear"]
              [callback (lambda (i e)
                          (set-cell-ht! (make-hash))
                          (set-cell-buf! (make-hash))
                          (reset-num-generations!)
                          (set-sim-running! #f)
                          (send textfield-generations set-value
                                (number->string num-generations))
                          (send board-canvas refresh))]))
(define button-random
  (new button% [parent control-panel-top]
               [label "Random"]
               [callback (lambda (i e)
                           (cell-seed cell-ht)
                           (draw-board cell-ht))]))

; ****************************** MIDDLE CONTROL PANEL *****************************
(define control-panel-mid (new horizontal-panel%
                               [parent main-frame]
                               [alignment '(center center)]
                               [stretchable-height #f]
                               [stretchable-width #f]))

; Control panel GUI elements (bottom)
(define slider-speed
  (new slider% [parent control-panel-mid]
               [label "Speed"]
               [style '(plain horizontal)]
               [min-value -20000]
               [max-value 0]
               [init-value -5000]
               [callback (lambda (i e)
                           (set-sleep-delay!
                            (abs (/ (send slider-speed get-value) 50000))))]))

(define slider-cell-size
  (new slider% [parent control-panel-mid]
               [label "Cell Size"]
               [style '(plain horizontal)]
               [min-value MIN-CELL-LENGTH]
               [max-value MAX-CELL-LENGTH]
               [init-value INIT-CELL-LENGTH]
               [callback (lambda (i e)
                           (let* ((new-cell-length (send slider-cell-size get-value))
                                  (curr-cell-length cell-length)
                                  (dx (round (/ (- (/ board-width new-cell-length)
                                                   (/ board-width curr-cell-length)) 2)))
                                  (dy (round (/ (- (/ board-height new-cell-length)
                                                   (/ board-height curr-cell-length)) 2))))
                             (set-cell-ht! (cell-offset cell-ht dx dy))
                             (set-cell-length! new-cell-length)
                             (update-max-xy)
                             (cond ((and (< cell-length 4)
                                         (equal? (send (send dc get-pen) get-style) 'solid))
                                    (change-cell-border 'transparent)
                                    (send checkbox-border set-value #f)))
                             (send board-canvas refresh)))]))

; ***************************** BOTTOM CONTROL PANEL ******************************
; Bottom control panel
(define control-panel-bottom (new horizontal-panel%
                                  [parent main-frame]
                                  [alignment '(center center)]
                                  [stretchable-height #f]
                                  [stretchable-width #f]))

(define textfield-generations
  (new text-field% [parent control-panel-bottom]
                   [label "Generations"]))

(define choice-cell-color
  (new choice% [parent control-panel-bottom]
               [label "Cell Color "]
               [choices '("Yellow" "Red" "Green" "DodgerBlue" "Orange" "White" "Magenta")]
               [callback (lambda (i e)
                           (set-cell-alive-color!
                           (send choice-cell-color get-string-selection))
                           (draw-board cell-ht))]))

(define checkbox-border
  (new check-box% [parent control-panel-bottom]
                  [label "Cell Border"]
                  [callback (lambda (i e)
                              (cond ((equal? #t (send checkbox-border get-value))
                                     (change-cell-border 'solid))
                                    (else
                                     (change-cell-border 'transparent))))]))

; *********************************** DRAWING *************************************
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

; Changes the border style of each cell
(define (change-cell-border style)
  (send dc set-pen (make-object pen% color-dead 1 style))
  (draw-board cell-ht))

; ******************************** INITIALIZATION *********************************
(send main-frame show #t)
(sleep/yield 0)
(send board-canvas refresh)
(send board-canvas focus)
(update-max-xy)
(send dc set-pen (make-object pen% color-dead 1 cell-border-style))
(send textfield-generations set-value (number->string num-generations))
(collect-garbage)

; ********************************** GAME LOOP ************************************
; Produces one iteration of the game
(define (game-one-iter)
  (begin
    (init-ht (cell-active cell-ht) cell-ht cell-buf)
    (board-next-gen cell-ht cell-buf)
    (draw-board (hash-diff cell-ht cell-buf))
    (set-cell-ht! (hash-copy (cell-remove-dead cell-buf)))
    (set-cell-buf! (make-hash))
    (increment-num-generations!)
    (send textfield-generations set-value (number->string num-generations))
    (collect-garbage 'incremental)))

; Main loop
(define (game-loop)
    (cond ((equal? sim-running #t)
           (game-one-iter)
           (sleep/yield sleep-delay)
           (game-loop))))

; Draw a gosper gun
(define (draw-gosper)
  (begin
    (for ([(key value) (in-hash gosper)])
      (hash-set! cell-ht key value))
    (draw-board cell-ht)))


