#lang racket

(require "matrix.rkt")
(require "renderer.rkt")
(require "scratch.rkt")
(provide (all-defined-out))

(struct renderable
  (triangles position rotation scale)
  #:transparent
  #:mutable)

(require math/matrix)

(define (draw-triangle p0 p1 p2)
  (define v0 (point- p1 p0))
  (define v1 (point- p2 p0))
  (if (>=  (point-ref(cross-product v0 v1) 2) 0)
      (draw-path (list p0 p1 p2 p0))
      (void)))

(define (draw-renderable r)
  
  (define transform
    (matrix*
     (apply translation-matrix (renderable-position r))
     (apply rotation-matrix (renderable-rotation r))
     (apply scale-matrix (renderable-scale r))))
  
  (for ([triangle (renderable-triangles r)])
    (apply 
     draw-triangle
     (map (lambda (point) (matrix* transform point))
          triangle))))

(define (prism x y z)
  (renderable 
   cube-triangles
   '(0 0 0) '(0 0 0) (list (/ x 2) (/ y 2) (/ z 2))))

(define (triangulate-rect p0 p1 p2 p3)
  (list (list p0 p1 p2) (list p0 p2 p3)))

(define (triangulate-path path0 path1)
  (apply
   append
   (for/list
       ([p0 path0]
        [p1 (cdr path0)]
        [p3 path1]
        [p2 (cdr path1)])
     (triangulate-rect p0 p1 p2 p3))))

(define (sphere r)
  (define paths
    (for/list ([i (in-range 17)])
      (define phi (/ (* 2 pi i) 16))
      (for/list ([j (in-range 9)])
        (define theta (/ (* pi j) 8))
        (make-point 
         (* (sin theta) (cos phi))
         (* (sin theta) (sin phi))
         (cos theta) ))))
  (define triangles
    (apply 
     append 
     (for/list ([path0 paths][path1 (cdr paths)])
       (triangulate-path path0 path1))))
  (renderable triangles
              '(0 0 0) '(0 0 0) (list r r r)))

(define (torus R r)
  (define paths
    (for/list ([i (in-range 17)])
      (define phi (/ (* 2 pi i) 16))
      (for/list ([j (in-range 17)])
        (define theta (/ (* 2 pi j) 16))
        (make-point 
         (* (+ R (* r (cos phi))) (cos theta)) 
         (* (+ R (* r (cos phi))) (sin theta))
         (* r (sin phi))))))
  (define triangles
    (apply 
     append 
     (for/list ([path0 paths][path1 (cdr paths)])
       (triangulate-path path0 path1))))
  (renderable triangles
              '(0 0 0) '(0 0 0) '(1 1 1)))

(define calls 0)
(define p (torus 100 30))
(set-renderable-position! p '(100 100 100))
(set-renderable-rotation! p (list 3 1 .5))
(clear)
(draw-renderable p)
target