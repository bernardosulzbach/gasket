;; Procedures used to approximate pi with high precision.
;;
;; π is defined as a constant.

#lang racket

(require test-engine/racket-tests)

;; slow-approximate-π : PositiveInteger -> Number
;; Slowly approximates π.
(define (slow-approximate-π steps)
  ;; pi² / 6 = (sum-first-inverse-squares steps)
  (sqrt (* 6 (sum-first-inverse-squares steps))))

;; Sums the first n inverse squares (e.g.: 1/1¹ + 1/2² + ...)
(define (sum-first-inverse-squares n)
  (if (= n 1) 1 (+ (sum-first-inverse-squares (sub1 n)) (/ 1 (sqr n)))))

(check-expect (sum-first-inverse-squares 1) (/ 1 1))
(check-expect (sum-first-inverse-squares 2) (/ 5 4))
(check-expect (sum-first-inverse-squares 3) (/ 49 36))
(check-expect (sum-first-inverse-squares 4) (/ 205 144))

(define (sum-fast-approximate-pi n)
  (if (= n 0) 1 (+ (sum-fast-approximate-pi (sub1 n)) (fast-approximation-step n))))

(define (fast-approximation-step n)
  (* (/ (double-factorial (* 2 n)) (double-factorial (+ (* 2 n) 1))) (/ 1 (expt 2 n))))

;; double-factorial : Integer -> Integer
;; The double factorial (!!) of n = n(n - 2)!! if n > 1 else 1
(define (double-factorial n)
  (if (or (= 0 n) (= 1 n)) 1 (* n (double-factorial (- n 2)))))

(check-expect (double-factorial 0) 1)
(check-expect (double-factorial 1) 1)
(check-expect (double-factorial 2) 2)
(check-expect (double-factorial 3) 3)
(check-expect (double-factorial 4) 8)

;; fast-approximate-π : PositiveInteger -> Number
;; Fastly approximates π.
(define (fast-approximate-pi steps)
  ;; pi / 2 = (sum-fast-approximate-pi steps)
  (* 2 (sum-fast-approximate-pi steps)))

;; π : Rational
;; A decent rational approximation of π.
(define π (fast-approximate-pi 512))

(test)
