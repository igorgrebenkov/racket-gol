#lang racket

; Contains all global variables used in the game

(require racket/gui)
(provide (all-defined-out))

; *********************************** CONSTANTS ***********************************
(define INIT-FRAME-HEIGHT 580)
(define INIT-FRAME-WIDTH 750)
(define INIT-CELL-LENGTH 5)
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
(define cell-alive-brush (make-object brush% color-alive cell-inner-style))
(define cell-dead-brush (make-object brush% color-dead cell-inner-style))

; ******************************** STATE VARIABLES ********************************
; Dimension related
(define cell-length INIT-CELL-LENGTH)  
(define frame-height INIT-FRAME-HEIGHT)      
(define frame-width INIT-FRAME-WIDTH)
(define sleep-delay INIT-SLEEP-DELAY)
(define max-x
  (exact-round (/ frame-width cell-length)))
(define max-y
  (exact-round (/ frame-height cell-length)))

; Hash table buffers
(define cell-ht (make-hash))     
(define cell-buf (make-hash))
(define cell-neighbor (make-hash))

; General game state
(define num-generations 0)
(define sim-running #f)

; Cell styling
(define cell-border-style 'transparent)
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

(define (set-frame-height! height) (set! frame-height height))

(define (set-frame-width! width) (set! frame-width width))

(define (set-max-x! x) (set! max-x x))

(define (set-max-y! y) (set! max-y y))

(define (toggle-border-style!)
  (cond ((equal? cell-border-style 'solid)
         (set! cell-border-style 'transparent))
        (else
         (set! cell-border-style 'solid))))