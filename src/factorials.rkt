; Procedures used to approximate pi with high precision.
;
; π is defined as a constant.

#lang racket

(require test-engine/racket-tests)

; Integer -> Integer
; The factorial of n:
;  1             if n = 0
;  n * (n - 1)!  otherwise
(define (factorial n)
  (factorial-internal n 1))

; Tail-recursion for factorial.
(define (factorial-internal n current)
  (if (zero? n) current (factorial-internal (sub1 n) (* n current))))

(check-expect (factorial 0) 1)
(check-expect (factorial 1) 1)
(check-expect (factorial 2) 2)
(check-expect (factorial 3) 6)
(check-expect (factorial 4) 24)
(check-expect (factorial 5) 120)
(check-expect (factorial 6) 720)

; Integer -> Integer
; The double factorial of n:
;  1              if n = 0 or n = 1
;  n * (n - 2)!!  otherwise
(define (double-factorial n)
  (double-factorial-internal n 1))

; Tail-recursion for double-factorial.
(define (double-factorial-internal n current)
  (if (or (= 0 n) (= 1 n)) current (double-factorial-internal (- n 2) (* n current))))

(check-expect (double-factorial 0) 1)
(check-expect (double-factorial 1) 1)
(check-expect (double-factorial 2) 2)
(check-expect (double-factorial 3) 3)
(check-expect (double-factorial 4) 8)

(test)

(provide factorial)
(provide double-factorial)