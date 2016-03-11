;; A collection of small utilities for Racket written by Bernardo Sulzbach.
;;
;; Email mafagafogigante@gmail.com if you have any doubts or suggestions.
;;
;; Made public under the BSD 2-Clause license.

#lang racket

(require math)
(require test-engine/racket-tests)

;; Applies a function to a value the specified number of times
(define (apply function value times)
  (if (< times 1)
      value
      (apply function (function value) (sub1 times))))

;; Returns the initial Fibonacci matrix [[1 1], [1, 0]]
(define (fibonacci-initial-matrix)
  (matrix [[1 1] [1 0]]))

;; nth-fibonacci : (Nonnegative Integer) -> (Nonnegative Integer)
;; Returns the n-th Fibonacci number
;;
(define (nth-fibonacci n)
  (define (multiply-by-fibonacci-initial-matrix matrix) (matrix* matrix (fibonacci-initial-matrix)))
  (matrix-ref (apply multiply-by-fibonacci-initial-matrix (fibonacci-initial-matrix) n) 1 1))

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
;;   (divides 1 0) -> true
;;   (divides 1 1) -> true
;;   (divides 2 0) -> true
;;   (divides 2 1) -> false
;;   (divides 2 2) -> true
;;   (divides 2 3) -> false
(define (divides divisor number)
  (zero? (modulo number divisor)))

(check-expect (divides 1 0) true)
(check-expect (divides 1 1) true)
(check-expect (divides 1 43) true)
(check-expect (divides 1 829) true)
(check-expect (divides 1 7829) true)

(check-expect (divides 2 0) true)
(check-expect (divides 2 1) false)
(check-expect (divides 2 2) true)
(check-expect (divides 2 3) false)
(check-expect (divides 2 4) true)

(check-expect (divides 3 0) true)
(check-expect (divides 3 1) false)
(check-expect (divides 3 2) false)
(check-expect (divides 3 3) true)
(check-expect (divides 3 4) false)

(struct 2DPoint (x y))

(define (distance point-a point-b)
  (sqrt (+ (sqr (- (2DPoint-x point-a) (2DPoint-x point-b))) (- (2DPoint-y point-a) (2DPoint-y point-b)))))

;; within-circle : Number Number Number Number Number -> Boolean
;; Evaluates whether or not a a point is inside a circle. The two first numbers represent the X and Y coordinates of the circle.
;; The third number is the circle radius. Finally, the last two numbers represent the X and Y coordinates of point you are testing.
(define (within-circle center radius point)
  (< (distance center point) radius))

(check-expect (within-circle (2DPoint 0 0) 2 (2DPoint 0 0)) #t)
(check-expect (within-circle (2DPoint 0 0) 2 (2DPoint 1 1)) #t)
(check-expect (within-circle (2DPoint 0 0) 2 (2DPoint 2 2)) #f)
(check-expect (within-circle (2DPoint 0 0) 2 (2DPoint -1 -1)) #t)

(check-expect (within-circle (2DPoint 2 2) 1 (2DPoint 2 2)) #t)
(check-expect (within-circle (2DPoint 2 2) 1 (2DPoint 2 3)) #t)
(check-expect (within-circle (2DPoint 2 2) 1 (2DPoint 3 3)) #f)

(test)
