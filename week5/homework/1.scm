#lang racket

;; 2.24
(list 1 (list 2 (list 3 4)))

; result: (1 (2 (3 4)))
; box and pointer
; -> [*][*]->[*][/]
;     |       |
;     1      [*][*]->[*][/]
;             |       |
;             2      [*][*]->4
;                     |
;                     3
;tree
;           *
;          / \
;         /   \
;        1     *
;             / \
;            /   \
;           2     *
;                / \
;               3   4

;; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ; -> (1 2 3 4 5 6)
(cons x y)   ; -> ((1 2 3) 4 5 6)
(list x y)   ; -> ((1 2 3) (4 5 6))

;; 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

;b
(define test-mobile (make-mobile (make-branch 3 2) (make-branch 4 5))) ; weight of this is 7

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? (branch-structure (right-branch mobile))
        ((not (pair? (branch-structure mobile))) (branch-structure mobile))
        (else (+ (total-weight (right-branch mobile))
                 (total-weight (left-branch mobile))))))


(total-weight test-mobile)
;(total-weight (make-mobile (make-branch 2 (make-branch 3 4)) (make-mobile 4 5)))
;(total-weightt test-mobile)
