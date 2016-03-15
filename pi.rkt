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
  (* (/ (!! (* 2 n)) (!! (+ (* 2 n) 1))) (/ 1 (expt 2 n))))

;; ! : Integer -> Integer
;; The factorial of n:
;;  1             if n = 0
;;  n * (n - 1)!  otherwise
(define (! n)
  (if (= 0 n) 1 (* n (! (sub1 n)))))

(check-expect (! 0) 1)
(check-expect (! 1) 1)
(check-expect (! 2) 2)
(check-expect (! 3) 6)
(check-expect (! 4) 24)
(check-expect (! 5) 120)
(check-expect (! 6) 720)

;; !! : Integer -> Integer
;; The double factorial of n:
;;  1              if n = 0 or n = 1
;;  n * (n - 2)!!  otherwise
(define (!! n)
  (if (or (= 0 n) (= 1 n)) 1 (* n (!! (- n 2)))))

(check-expect (!! 0) 1)
(check-expect (!! 1) 1)
(check-expect (!! 2) 2)
(check-expect (!! 3) 3)
(check-expect (!! 4) 8)

;; fast-approximate-π : PositiveInteger -> Number
;; Fastly approximates π.
(define (fast-approximate-pi steps)
  ;; pi / 2 = (sum-fast-approximate-pi steps)
  (* 2 (sum-fast-approximate-pi steps)))

;; π : Rational
;; A decent rational approximation of π.
(define π (fast-approximate-pi 512))

(test)
