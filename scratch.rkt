#lang racket

(require math/matrix)
(require "matrix.rkt")

(provide (all-defined-out))

(define t (list
           (list 
            (col-matrix [-1 1 -1 1])
            (col-matrix [1 1 -1 1])
            (col-matrix [-1 -1 -1 1]))
           (list 
            (col-matrix [1 1 -1 1])
            (col-matrix [1 -1 -1 1])
            (col-matrix [-1 -1 -1 1]))))

(define cube-triangles
  (append
   (apply 
    append
    (for/list ([r (list 0 (/ pi 2) pi (* 3(/ pi 2)))])
      (define trans (rotation-matrix 0 r 0))
      (for/list ([tr t])
        (map (lambda (x) (matrix* trans x)) tr))))
   (apply 
    append
    (for/list ([r (list (/ pi 2) (* 3(/ pi 2)))])
      (define trans (rotation-matrix r 0 0))
      (for/list ([tr t])
        (map (lambda (x) (matrix* trans x)) tr))))
   ))