;; A collection of small utilities for Racket written by Bernardo Sulzbach.
;;
;; Email mafagafogigante@gmail.com if you have any doubts or suggestions.
;;
;; Made public under the BSD 2-Clause license.

#lang racket

(require math)
(require test-engine/racket-tests)

;; repeat : Function Value Number -> Value
;; Applies a function to a value the specified number of times.
(define (repeat function value times)
  (if (< times 1)
      value
      (repeat function (function value) (sub1 times))))

;; Returns the initial Fibonacci matrix [[1 1], [1, 0]]
(define (fibonacci-initial-matrix)
  (matrix [[1 1] [1 0]]))

;; nth-fibonacci : (Nonnegative Integer) -> (Nonnegative Integer)
;; Returns the n-th Fibonacci number
;;
(define (nth-fibonacci n)
  (define (multiply-by-fibonacci-initial-matrix matrix) (matrix* matrix (fibonacci-initial-matrix)))
  (matrix-ref (matrix-expt (fibonacci-initial-matrix) n) 0 1))

(check-expect (nth-fibonacci 0) 0)
(check-expect (nth-fibonacci 1) 1)
(check-expect (nth-fibonacci 2) 1)
(check-expect (nth-fibonacci 3) 2)
(check-expect (nth-fibonacci 4) 3)
(check-expect (nth-fibonacci 5) 5)
(check-expect (nth-fibonacci 6) 8)
(check-expect (nth-fibonacci 7) 13)
(check-expect (nth-fibonacci 8) 21)
(check-expect (nth-fibonacci 9) 34)
(check-expect (nth-fibonacci 10) 55)
(check-expect (nth-fibonacci 20) 6765)
(check-expect (nth-fibonacci 30) 832040)
(check-expect (nth-fibonacci 40) 102334155)
(check-expect (nth-fibonacci 50) 12586269025)
(check-expect (nth-fibonacci 60) 1548008755920)
(check-expect (nth-fibonacci 70) 190392490709135)
(check-expect (nth-fibonacci 80) 23416728348467685)
(check-expect (nth-fibonacci 90) 2880067194370816120)
(check-expect (nth-fibonacci 100) 354224848179261915075)

;; divides : (Positive Integer) (Nonnegative Integer) -> Boolean
;; Evaluates whether or not a number divides another.
;; Examples:
;;   (divides 1 0) -> #t
;;   (divides 1 1) -> #t
;;   (divides 2 0) -> #t
;;   (divides 2 1) -> #f
;;   (divides 2 2) -> #t
;;   (divides 2 3) -> #f
(define (divides divisor number)
  (zero? (modulo number divisor)))

(check-expect (divides 1 0) #t)
(check-expect (divides 1 1) #t)
(check-expect (divides 1 43) #t)
(check-expect (divides 1 829) #t)
(check-expect (divides 1 7829) #t)

(check-expect (divides 2 0) #t)
(check-expect (divides 2 1) #f)
(check-expect (divides 2 2) #t)
(check-expect (divides 2 3) #f)
(check-expect (divides 2 4) #t)

(check-expect (divides 3 0) #t)
(check-expect (divides 3 1) #f)
(check-expect (divides 3 2) #f)
(check-expect (divides 3 3) #t)
(check-expect (divides 3 4) #f)

;; A point in a bi-dimensional plane.
(struct 2DPoint (x y))

;; distance : 2DPoint 2DPoint -> Number
;; Evaluates the distance between two points.
(define (distance point-a point-b)
  ;; Takes the square root of the sum of the square of the differences.
  (sqrt
   (apply +
    (map
     (lambda (f) (sqr (apply - (map f (list point-a point-b)))))
     (list 2DPoint-x 2DPoint-y)))))

;; within-circle : Number Number Number Number Number -> Boolean
;; Evaluates whether or not a a point is in a circle. The two first numbers represent the X and Y coordinates of the circle.
;; The third number is the circle radius. Finally, the last two numbers represent the X and Y coordinates of point you are testing.
(define (within-circle center radius point)
  (<= (distance center point) radius))

(check-expect (within-circle (2DPoint 0 0) 2 (2DPoint 0 0)) #t)
(check-expect (within-circle (2DPoint 0 0) 2 (2DPoint 1 1)) #t)
(check-expect (within-circle (2DPoint 0 0) 2 (2DPoint 2 2)) #f)
(check-expect (within-circle (2DPoint 0 0) 2 (2DPoint -1 -1)) #t)

(check-expect (within-circle (2DPoint 2 2) 1 (2DPoint 2 2)) #t)
(check-expect (within-circle (2DPoint 2 2) 1 (2DPoint 2 3)) #t)
(check-expect (within-circle (2DPoint 2 2) 1 (2DPoint 3 3)) #f)

;; Function List -> Integer
;; Returns the number of milliseconds the function takes to finish with the provided list of arguments.
(define (wall-time-apply procedure arguments)
  (let-values ([(timings cpu wall gc) (time-apply procedure arguments)])
    cpu))

(test)
