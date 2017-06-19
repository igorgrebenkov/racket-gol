#lang racket

; Contains the game view, I/O and Main loop

(require racket/gui)
(require racket/draw)
(require "global-vars.rkt")
(require "cells.rkt")

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
    (define/override (on-paint)                         
      (begin
        ; disabling this line looks cool
        (send board-canvas set-canvas-background color-background)
        (draw-board cell-ht)
        (let ((curr-frame-height (send main-frame get-height))
              (curr-frame-width (send main-frame get-width)))
        (cond ((not (and (equal? curr-frame-height INIT-FRAME-HEIGHT)  ; handles window resizing
                         (equal? curr-frame-width INIT-FRAME-WIDTH)))
               (set-frame-height! curr-frame-height)
               (set-frame-width! curr-frame-width)
               (set-max-x! (exact-round (/ frame-width cell-length)))
               (set-max-y! (exact-round (/ frame-height cell-length))))))))
    (super-new)))


; The main-frame holds the board-canvas and control panels
(define main-frame (new frame% [label "Game of Life"]
                               [height frame-height]
                               [width frame-width]))

; Represents the game board
(define board-canvas (new game-canvas%	 
                          [parent main-frame]	 
                          [style '()]))

; Top control panel
(define control-panel-top (new horizontal-panel%
                               [parent main-frame]
                               [alignment '(center center)]
                               [stretchable-height #f]
                               [stretchable-width #f]))

; Bottom control panel
(define control-panel-bottom (new horizontal-panel%
                                  [parent main-frame]
                                  [alignment '(center center)]
                                  [stretchable-height #f]
                                  [stretchable-width #f]))

; Control panel GUI elements
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
               [label "Next"]))

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

(define slider-speed
  (new slider% [parent control-panel-bottom]
               [label "Speed"]
               [style '(plain horizontal)]
               [min-value 0]
               [max-value 20000]
               [init-value 5000]
               [callback (lambda (i e)
                           (set-sleep-delay!
                            (/ (send slider-speed get-value) 100000)))]))

(define slider-cell-size
  (new slider% [parent control-panel-bottom]
               [label "Cell Size"]
               [style '(plain horizontal)]
               [min-value 1]
               [max-value 30]
               [init-value 10]
               [callback (lambda (i e)
                           (send board-canvas refresh)
                           (set-cell-length! (send slider-cell-size get-value))    
                           (set-max-x!
                            (exact-round (/ frame-width cell-length)))
                           (set-max-y!
                            (exact-round (/ frame-height cell-length))))]))

(define textfield-generations
  (new text-field% [parent control-panel-top]
                   [label "Generations"]))

(define checkbox-border
  (new check-box% [parent control-panel-bottom]
                  [label "Cell Border"]
                  [callback (lambda (i e)
                              (toggle-border-style!)
                              (send dc set-pen
                                    (make-object pen% color-dead 1 cell-border-style))
                              (draw-board cell-ht))]))



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
(send main-frame show #t)
(sleep/yield 0)
(send board-canvas refresh)
(send board-canvas focus)
(send dc set-pen (make-object pen% color-dead 1 cell-border-style))
(send textfield-generations set-value (number->string num-generations))
(collect-garbage)

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

; Randomly seeds the board
(define (cell-seed ht)
  (for ([i (in-range 0 max-x)])
    (for ([j (in-range 0 max-y)])
    (cond ((equal? (random 12) 0)
           (hash-set! ht (list i j) ALIVE))))
  (draw-board ht)))

; Draw a gosper gun
(define (draw-gosper)
  (begin
    (for ([(key value) (in-hash gosper)])
      (hash-set! cell-ht key value))
    (draw-board cell-ht)))


