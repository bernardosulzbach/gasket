;; A collection of small utilities for Racket written by Bernardo Sulzbach.
;;
;; Email mafagafogigante@gmail.com if you have any doubts or suggestions.
;;
;; Made public under the BSD 3-Clause license.

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

;; fibonacci : (Nonnegative Integer) -> (Nonnegative Integer)
;; Returns the n-th Fibonacci number
;;
(define (fibonacci n)
  (define (multiply-by-fibonacci-initial-matrix matrix) (matrix* matrix (fibonacci-initial-matrix)))
  (matrix-ref (apply multiply-by-fibonacci-initial-matrix (fibonacci-initial-matrix) n) 1 1))

(check-expect (fibonacci 0) 0)
(check-expect (fibonacci 1) 1)
(check-expect (fibonacci 2) 1)
(check-expect (fibonacci 3) 2)
(check-expect (fibonacci 4) 3)
(check-expect (fibonacci 5) 5)
(check-expect (fibonacci 6) 8)
(check-expect (fibonacci 7) 13)
(check-expect (fibonacci 8) 21)
(check-expect (fibonacci 9) 34)
(check-expect (fibonacci 10) 55)
(check-expect (fibonacci 20) 6765)
(check-expect (fibonacci 30) 832040)
(check-expect (fibonacci 40) 102334155)
(check-expect (fibonacci 50) 12586269025)
(check-expect (fibonacci 60) 1548008755920)
(check-expect (fibonacci 70) 190392490709135)
(check-expect (fibonacci 80) 23416728348467685)
(check-expect (fibonacci 90) 2880067194370816120)
(check-expect (fibonacci 100) 354224848179261915075)

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

(test)
