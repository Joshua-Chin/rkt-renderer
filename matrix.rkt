#lang racket

(require math/matrix)

(provide (all-defined-out))
(provide matrix*)

(define (make-point x y [z 0] [w 1])
  (col-matrix [x y z w]))

(define (point-ref point index)
  (matrix-ref point index 0))

(define point- matrix-)
(define point-dot matrix-dot)

(define (cross-product p0 p1)
  (let
      ([u (lambda (x) (point-ref p0 x))]
       [v (lambda (x) (point-ref p1 x))])
    (col-matrix
     [(- (* (u 1) (v 2)) (* (u 2) (v 1)))
      (- (* (u 2) (v 0)) (* (u 0) (v 2)))
      (- (* (u 0) (v 1)) (* (u 1) (v 0)))
      0])))

(define (scale-matrix x y z)
  (matrix
   [[x 0 0 0]
    [0 y 0 0]
    [0 0 z 0]
    [0 0 0 1]]))

(define (translation-matrix x y z)
  (matrix
   [[1 0 0 x]
    [0 1 0 y]
    [0 0 1 z]
    [0 0 0 1]]))

(define (rotation-matrix x y z)
  (let
      ([cx (cos x)]
       [cy (cos y)]
       [cz (cos z)]
       [sx (sin x)]
       [sy (sin y)]
       [sz (sin z)])
    (matrix*
     (matrix
      [[1       0 0 0]
       [0 cx (- sx) 0]
       [0 sx     cx 0]
       [0  0      0 1]])
     (matrix
      [[    cy 0 sy 0]
       [     0 1  0 0]
       [(- sy) 0 cy 0]
       [     0 0  0 1]])
     (matrix
      [[cz (- sz) 0 0]
       [sz     cz 0 0]
       [0       0 1 0]
       [0       0 0 1]]))))
