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

(define tree-1 (node 50 "Weber"
                     (node 15 "Ada"
                           (node 10 "Adam" null null)
                           (node 20 "Bernardo" null null))
                     (node 55 "Lazarus" null null)))

(define (number<? a b)
  (< a b))

(check-expect (binary-search tree-1 0 number<?) null)
(check-expect (binary-search tree-1 25 number<?) null)
(check-expect (binary-search tree-1 75 number<?) null)
(check-satisfied (binary-search tree-1 50 number<?) (λ (node) (string=? (node-value node) "Weber")))
(check-satisfied (binary-search tree-1 15 number<?) (λ (node) (string=? (node-value node) "Ada")))
(check-satisfied (binary-search tree-1 10 number<?) (λ (node) (string=? (node-value node) "Adam")))
(check-satisfied (binary-search tree-1 20 number<?) (λ (node) (string=? (node-value node) "Bernardo")))
(check-satisfied (binary-search tree-1 55 number<?) (λ (node) (string=? (node-value node) "Lazarus")))

(test)