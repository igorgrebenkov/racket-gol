# racket-gol
A Conway's Game of Life implementation in Racket. 

It uses a hash-table based implementation that
only stores the state of active cells. This allows for an infinite grid, and results
in fast next generation computation for sparse grids.

![racket-gol screenshot][screenshot]

[screenshot]: https://github.com/igorgrebenkov/racket-gol/blob/master/screenshot.gif

## Features
* Infinite, pannable/zoomable grid
* Live drawing/deleting of active cells
* Adjustable generation speed
* 7 cell color options
* Toggleable cell border


## Controls
```
Left Click + Drag         - draw cells
Ctrl + LeftClick + Drag   - delete cells
Scroll Wheel              - zoom in/out
Right Click + Drag        - pan board
```
