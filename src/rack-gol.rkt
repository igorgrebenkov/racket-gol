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
    (define/override (on-char event)                     ; Keyboard events
      (let ((key-char (send event get-key-code)))
           (cond ((equal? key-char 'f1) (begin (start-loop)))
                 ((equal? key-char 'f2)
                  ;(draw-gosper)
                  )
                 ((equal? key-char 'f3)
                  (send board-canvas refresh)
                  (set-cell-length! (+ cell-length 0.3))
                  (set-max-x!
                    (exact-round (/ frame-width cell-length)))
                  (set-max-y!
                    (exact-round (/ frame-height cell-length))))
                 ((equal? key-char 'f4)
                  (send board-canvas refresh)
                  (set-cell-length! (- cell-length 0.3))
                  (set-max-x!
                    (exact-round (/ frame-width cell-length)))
                  (set-max-y!
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
               (set-frame-height! curr-frame-height)
               (set-frame-width! curr-frame-width)
               (set-max-x! (exact-round (/ frame-width cell-length)))
               (set-max-y! (exact-round (/ frame-height cell-length))))))))
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

; Produces one iteration of the game
(define (game-one-iter)
  (begin
    (init-ht (cell-active cell-ht) cell-ht cell-buf)
    (board-next-gen cell-ht cell-buf)
    (draw-board (hash-diff cell-ht cell-buf))
    (set-cell-ht! (hash-copy (cell-remove-dead cell-buf)))
    (set-cell-buf! (make-hash))
    (collect-garbage 'incremental)))

; Main loop
(define (start-loop)
    (for ([i (in-range 0 +inf.0)])
      (game-one-iter)
      (sleep/yield sleep-delay)))

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


