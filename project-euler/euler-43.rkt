#lang racket

(require test-engine/racket-tests)
(require racket/set)

;; See https://projecteuler.net/problem=43 for a full description of the problem.

;; Solution
;;
;; Using a divide-and-conquer approach, I break down the problem into smaller ones by first solving
;; for which digit combinations make the first proposition valid. From there, it is just a matter of
;; iterating down into which next digits will make the next preposition valid and so on until the
;; complete pandigital number is formed.

;; get-proposition-divider : integer -> integer
;; Returns the divider used by a given proposition.
(define (get-proposition-divider proposition)
  (vector-ref (vector 2 3 5 7 11 13 17) (sub1 proposition)))

;; divides : Integer Integer -> Boolean
(define (divides divider number)
  (= 0 (modulo number divider)))

(check-expect (divides 2 3) #f)
(check-expect (divides 2 4) #t)
(check-expect (divides 7 14) #t)
(check-expect (divides 7 43) #f)
(check-expect (divides 3 10625) #f)

;; check-proposition : String Integer -> Boolean
;; Evaluates whether or not a proposition holds for a given full number.
;; Only the last three digits of the number are taken into account.
(define (check-proposition combination proposition)
  (divides (get-proposition-divider proposition) (modulo (string->number combination) 1000)))

(check-expect (check-proposition "1000" 1) #t)
(check-expect (check-proposition "1001" 1) #f)
(check-expect (check-proposition "1024" 1) #t)
(check-expect (check-proposition "10624" 2) #t)
(check-expect (check-proposition "10625" 2) #f)

;; unused-characters : String -> Listof Character
;; Makes a list of the unused digits.
(define (unused-characters combination)
  (remove* (string->list combination) (string->list "0123456789")))

(define (append-all-possible-digits combination)
  (map (λ (digit) (apply string (append (string->list combination) (list digit))))
       (unused-characters combination)))

(define (has-valid-ending combination)
  (or (> 4 (string-length combination))
      (check-proposition combination (- (string-length combination) 3))))

(check-expect (has-valid-ending "10") #t)
(check-expect (has-valid-ending "100") #t)
(check-expect (has-valid-ending "1000") #t)
(check-expect (has-valid-ending "1001") #f)
(check-expect (has-valid-ending "1024") #t)
(check-expect (has-valid-ending "10624") #t)
(check-expect (has-valid-ending "10625") #f)

;; derive-interesting-combinations : String -> Listof String
;; Given a combination, this function derives all possible interesting combinations from it.
(define (derive-interesting-combinations combination)
  (filter has-valid-ending (append-all-possible-digits combination)))

;; sum-combinations : Listof String -> Integer
(define (sum-combinations combinations)
  (apply + (map string->number combinations)))

;; are-terminal-combinations : Listof String -> Boolean
(define (are-terminal-combinations combinations)
  (and (cons? combinations) (= 10 (string-length (first combinations)))))

;; sum-interesting-derived-combinations : List -> Integer
;; Given a list of integers for which all prepositions that apply are valid, returns the sum of the
;; integers for which all the prepositions hold that may be derived from the provided integers.
(define (sum-interesting-derived-combinations combinations)
  (if (are-terminal-combinations combinations)
      (sum-combinations combinations)
      (apply + (map sum-interesting-derived-combinations
                    (map derive-interesting-combinations combinations)))))

(define (solve)
  (sum-interesting-derived-combinations (list "1" "2" "3" "4" "5" "6" "7" "8" "9")))

(check-expect (solve) 16695334890)

(test)
