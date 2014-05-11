#lang racket

(require racket/draw)
(require "matrix.rkt")

(provide (all-defined-out))

(define target (make-bitmap 400 400))
(define dc (new bitmap-dc% [bitmap target]))

(define (draw-line p0 p1)
  (send dc draw-line
        (point-ref p0 0)
        (point-ref p0 1)
        (point-ref p1 0)
        (point-ref p1 1)))

(define (draw-path points)
  (for ([p0 points][p1 (cdr points)])
    (draw-line p0 p1)))

(define (clear)
  (send dc clear))