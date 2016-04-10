#lang racket

(require test-engine/racket-tests)

(struct node [key value a b])
; A Node element is a structure (node node-a node-b) where
; node-a is either a Node element or null
; node-b is either a Node element or null

; Performs binary search on a binary search tree trying to find a node with the provided key.
; If found, returns such node. Otherwise, returns null.
(define (binary-search node key less-than?)
  (if (null? node)
      null
      (cond
        [(less-than? (node-key node) key)
         (binary-search (node-b node) key less-than?)]
        [(less-than? key (node-key node))
         (binary-search (node-a node) key less-than?)]
        [else node])))

(define leaf-adam (node 10 "Adam" null null))
(define leaf-cohr (node 20 "Cohr" null null))
(define leaf-elmo (node 55 "Elmo" null null))
(define node-bach (node 15 "Bach" leaf-adam leaf-cohr))
(define root-dunn (node 50 "Dunn" node-bach leaf-elmo))

(define (number<? a b)
  (< a b))

(check-expect (binary-search root-dunn 0 number<?) null)
(check-expect (binary-search root-dunn 25 number<?) null)
(check-expect (binary-search root-dunn 75 number<?) null)
(check-expect (binary-search root-dunn 50 number<?) root-dunn)
(check-expect (binary-search root-dunn 15 number<?) node-bach)
(check-expect (binary-search root-dunn 10 number<?) leaf-adam)
(check-expect (binary-search root-dunn 20 number<?) leaf-cohr)
(check-expect (binary-search root-dunn 55 number<?) leaf-elmo)

(test)