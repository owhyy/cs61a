#lang racket
(require rackunit)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

;; we don't really have a set representation, so it will be a list
(define test-set '(1 2 3 4))

;; tests
(check-equal? (union-set test-set test-set) test-set "union test")
(check-equal? (union-set '(6) test-set) (adjoin-set 6 test-set) "union test")
(check-equal? (union-set '(1) test-set) test-set "union test")
