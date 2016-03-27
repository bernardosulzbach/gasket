; Geometric utilities for Racket.
;
; Written by Bernardo Sulzbach and licensed under BSD 2-Clause license.

#lang racket

(require math)
(require test-engine/racket-tests)

; A Point is a structure (point x y z)
; x, y, and z are Numbers
(struct point (x y z) #:transparent)

(check-expect (point-x (point 1 0 0)) 1)
(check-expect (point-y (point 1 0 0)) 0)
(check-expect (point-z (point 1 0 0)) 0)
(check-expect (point-x (point 1 2 0)) 1)
(check-expect (point-y (point 1 2 0)) 2)
(check-expect (point-z (point 1 2 0)) 0)
(check-expect (point-x (point 1 2 3)) 1)
(check-expect (point-y (point 1 2 3)) 2)
(check-expect (point-z (point 1 2 3)) 3)

; A Circle is a structure (circle point radius)
; center is a Point that represents the center of the Circle
; radius is a Number that represents the radius of the Circle
(struct circle (center radius) #:transparent)

; A Geo-vector is a structure (geo-vector point)
; point is a Point whose coordinates are the magnitudes of the Vector
(struct geo-vector (point) #:transparent)

; A Geo-plane is a structure (geo-plane geo-vector-b geo-vector-b)
; geo-vector-a and geo-vector-b are the Geo-vectors that define the Geo-plane
(struct geo-plane (geo-vector-a geo-vector-b))

; Point Point -> Number
; Evaluates the distance between two points.
(define (distance point-a point-b)
  ; Takes the square root of the sum of the square of the differences.
  (sqrt (+
         (sqr (- (point-x point-a) (point-x point-b)))
         (sqr (- (point-y point-a) (point-y point-b)))
         (sqr (- (point-z point-a) (point-z point-b))))))

; Circle Point -> Boolean
; Evaluates whether or not a Point is in a Circle.
(define (within-circle circle point)
  (<= (distance (circle-center circle) point) (circle-radius circle)))

(check-expect (within-circle (circle (point 0 0 0) 2) (point 0 0 0)) #t)
(check-expect (within-circle (circle (point 0 0 0) 2) (point 1 1 0)) #t)
(check-expect (within-circle (circle (point 0 0 0) 2) (point 2 2 0)) #f)
(check-expect (within-circle (circle (point 0 0 0) 2) (point -1 -1 0)) #t)
(check-expect (within-circle (circle (point 2 2 0) 1) (point 2 2 0)) #t)
(check-expect (within-circle (circle (point 2 2 0) 1) (point 2 3 0)) #t)
(check-expect (within-circle (circle (point 2 2 0) 1) (point 3 3 0)) #f)

; Prints a Geo-vector to the screen.
(define (geo-vector-print v)
  (printf "[~a ~a ~a]" (point-x (geo-vector-point v)) (point-y (geo-vector-point v)) (point-z (geo-vector-point v))))

; Geo-vector Geo-vector -> Geo-vector
; Returns the difference Geo-vector of two Geo-vectors.
(define (geo-vector-sub geo-vector-a geo-vector-b)
  (geo-vector (point
               (- (point-x (geo-vector-point geo-vector-a))
                  (point-x (geo-vector-point geo-vector-b)))
               (- (point-y (geo-vector-point geo-vector-a))
                  (point-y (geo-vector-point geo-vector-b)))
               (- (point-z (geo-vector-point geo-vector-a))
                  (point-z (geo-vector-point geo-vector-b))))))

(check-expect (geo-vector-sub (geo-vector (point 1 2 3)) (geo-vector (point 3 2 1))) (geo-vector (point -2 0 2)))

; List-of-point -> Geo-plane
; Derives a Geo-plane from the first three Points of a List-of-point.
(define (derive-geo-plane points)
  (geo-plane
   (geo-vector-sub (geo-vector (list-ref points 1)) (geo-vector (list-ref points 0)))
   (geo-vector-sub (geo-vector (list-ref points 2)) (geo-vector (list-ref points 0)))))

; Geo-plane -> Geo-vector
; Evaluates the normal vector of a Geo-plane.
(define (geo-plane-normal geo-plane)
  (geo-vector-cross-product (geo-plane-geo-vector-a geo-plane) (geo-plane-geo-vector-b geo-plane)))

(check-expect (geo-plane-normal
               (geo-plane (geo-vector (point 1 0 0)) (geo-vector (point 0 1 0))))
              (geo-vector (point 0 0 1)))

; Geo-vector Geo-vector -> Number
; Evaluates the dot product of two Geo-vectors.
(define (geo-vector-dot-product geo-vector-a geo-vector-b)
  (+ (* (point-x (geo-vector-point geo-vector-a))
        (point-x (geo-vector-point geo-vector-b)))
     (* (point-y (geo-vector-point geo-vector-a))
        (point-y (geo-vector-point geo-vector-b)))
     (* (point-z (geo-vector-point geo-vector-a))
        (point-z (geo-vector-point geo-vector-b)))))

; Geo-vector Geo-vector -> Geo-vector
; Evaluates the cross product of two Geo-vectors.
(define (geo-vector-cross-product geo-vector-a geo-vector-b)
  (geo-vector (point
               (- (*
                   (point-y (geo-vector-point geo-vector-a))
                   (point-z (geo-vector-point geo-vector-b)))
                   (*
                   (point-z (geo-vector-point geo-vector-a))
                   (point-y (geo-vector-point geo-vector-b))))
               (- (*
                   (point-z (geo-vector-point geo-vector-a))
                   (point-x (geo-vector-point geo-vector-b)))
                   (*
                   (point-x (geo-vector-point geo-vector-a))
                   (point-z (geo-vector-point geo-vector-b))))
               (- (*
                   (point-x (geo-vector-point geo-vector-a))
                   (point-y (geo-vector-point geo-vector-b)))
                   (*
                   (point-y (geo-vector-point geo-vector-a))
                   (point-x (geo-vector-point geo-vector-b)))))))

; Geo-plane Point -> Boolean
; Evaluates whether or not a Point is in a Geo-plane.
(define (in-plane plane point)
  ; A point is in a plane if its position vector is orthogonal to the normal of the plane.
  (zero? (geo-vector-dot-product (geo-plane-normal plane) (geo-vector point))))

; List-of-points -> Boolean
; Evaluates whether or not all points in a List-of-points are coplanar.
(define (are-coplanar list-of-points)
  (cond
    [(< (length list-of-points) 4) #t]
    [(andmap in-plane
             (derive-geo-plane (take list-of-points 3))
             (list-tail list-of-points 3))]))

(test)