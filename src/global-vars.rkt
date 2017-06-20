#lang racket

; Contains all global variables used in the game

(require racket/gui)
(provide (all-defined-out))

; *********************************** CONSTANTS ***********************************
(define INIT-FRAME-HEIGHT 580)
(define INIT-FRAME-WIDTH 750)
(define INIT-CELL-LENGTH 8)
(define MIN-CELL-LENGTH 1)
(define MAX-CELL-LENGTH 30)
(define INIT-SLEEP-DELAY (/ 1 10))

; General game state
(define ALIVE 1)
(define DEAD 0)
(define CONWAY-ALIVE-UPPER 3)
(define CONWAY-ALIVE-LOWER 2)
(define CONWAY-DEAD-THRESH 3)

; Cell styling
(define color-background (make-object color% 0 0 0))
(define color-dead "black")
(define color-alive "green")
(define cell-inner-style 'solid)
(define cell-border-style 'transparent)
(define cell-dead-brush (make-object brush% color-dead cell-inner-style))

; ******************************** STATE VARIABLES ********************************
; Dimension related
(define cell-length INIT-CELL-LENGTH)  
(define board-height 0)      
(define board-width 0)
(define max-x 0) 
(define max-y 0)
(define sleep-delay INIT-SLEEP-DELAY)

; Hash table buffers
(define cell-ht (make-hash))     
(define cell-buf (make-hash))
(define cell-neighbor (make-hash))

; General game state
(define num-generations 0)
(define sim-running #f)

; Cell styling
(define cell-alive-brush (make-object brush% color-alive cell-inner-style))

; Game rules
(define alive-upper-lim CONWAY-ALIVE-UPPER)
(define alive-lower-lim CONWAY-ALIVE-LOWER)
(define dead-thresh CONWAY-DEAD-THRESH)

; ******************************* MUTATOR FUNCTIONS *******************************
(define (set-cell-ht! ht) (set! cell-ht ht))

(define (set-cell-buf! ht) (set! cell-buf ht))

(define (set-cell-length! length) (set! cell-length length))

(define (set-sleep-delay! delay) (set! sleep-delay delay))

(define (increment-num-generations!)
  (set! num-generations (add1 num-generations)))

(define (reset-num-generations!) (set! num-generations 0))

(define (set-sim-running! value) (set! sim-running value))

(define (set-board-height! height) (set! board-height height))

(define (set-board-width! width) (set! board-width width))

(define (set-max-x! x) (set! max-x x))

(define (set-max-y! y) (set! max-y y))

(define (set-cell-alive-color! color-str)
  (set! cell-alive-brush (make-object brush% color-str cell-inner-style)))