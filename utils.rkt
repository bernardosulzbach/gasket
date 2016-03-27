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

;; nth-fibonacci : (NonnegativeInteger) -> (NonnegativeInteger)
;; Returns the n-th Fibonacci number
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

;; divides : (PositiveInteger) (NonnegativeInteger) -> Boolean
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

;; wall-time-apply : Function List -> Integer
;; Returns the number of milliseconds the function takes to finish with the provided list of arguments.
(define (wall-time-apply procedure arguments)
  (let-values ([(timings cpu wall gc) (time-apply procedure arguments)]) wall))

;; Σ : Function Integer Integer -> Any
;; Sums the results of the given function when mapped over all integers from the first passed integer up to the second, inclusive.
;; If the second integer is smaller than the first, returns 0, the result of an empty sum by definition.
(define (Σ function lower upper)
  (cond
    [(not (integer? lower)) (raise-argument-error 'Σ "integer?" lower)]
    [(not (integer? upper)) (raise-argument-error 'Σ "integer?" upper)]
    [else (for/sum ([x (in-range lower (add1 upper))]) (function x))]))

(check-expect (Σ (λ (x) (sqr x)) -1 -2) 0)
(check-expect (Σ (λ (x) (sqr x)) -1 -1) 1)
(check-expect (Σ (λ (x) (sqr x)) -1 0) 1)
(check-expect (Σ (λ (x) (sqr x)) 0 0) 0)
(check-expect (Σ (λ (x) (sqr x)) 0 1) 1)
(check-expect (Σ (λ (x) (sqr x)) 0 2) 5)
(check-expect (Σ (λ (x) (sqr x)) 0 3) 14)

;; ∫ : Function Number Number PositiveInteger -> Number
;; Numerically integrates the provided function over the provided range.
;; The PositiveInteger is the number of steps used. In general, the bigger the number of steps, the
;; better the approximation.
(define (∫ function lower upper steps)
  (simpsons-rule-approximate function lower upper steps))

(define (evaluate-simpsons-delta lower upper steps)
  (/ (- upper lower) steps))

(define (simpsons-rule-approximate function lower upper steps)
  (when (not (positive? steps)) (raise-argument-error 'simpsons-rule-approximate "positive?" steps))
  (* (/ (evaluate-simpsons-delta lower upper steps) 3)
     (simpsons-rule-sum function lower upper (evaluate-simpsons-delta lower upper steps))))

(define (simpsons-rule-sum function lower upper delta)
  (apply + (list (function lower)
                 (* 4 (for/sum ([x (in-range (+ lower delta) upper (* 2 delta))]) (function x)))
                 (* 2 (for/sum ([x (in-range (+ lower delta delta) (- upper delta) (* 2 delta))]) (function x)))
                 (function upper))))

(check-expect(simpsons-rule-approximate (λ (x) x) 0 4 10) 8)
(check-expect(simpsons-rule-approximate (λ (x) (expt x 2)) 0 6 10) 72)
(check-expect(simpsons-rule-approximate (λ (x) (expt x 3)) 0 10 10) 2500)

(test)
