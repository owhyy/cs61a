#lang racket
(require rackunit)

(define (element-of-set? x set)
  (not (false? (memq x set))))

(define (adjoin-set x set)
    (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set1 set2))

;; we don't really have a set representation, so it will be a list
(define test-set '(1 2 3 4))

;; tests
(check-equal? (element-of-set? 2 test-set) #t "element-of-set? #t test")
(check-equal? (element-of-set? 5 test-set) #f "element-of-set? #f test")

(check-equal? (adjoin-set 5 test-set) (cons 5 test-set))
(check-equal? (intersection-set test-set test-set) test-set "intersection-set test")

(check-equal? (union-set test-set test-set) (append test-set test-set) "union test")
(check-equal? (union-set '(6) test-set) (cons 6 test-set) "union test")
(check-equal? (union-set '(1) test-set) (cons 1 test-set) "union test")

;; this is going to be faster, because we don't have to check
;; if element already exists when doing union. however, there
;; isn't going to be much difference when doing intersection-set,
;; since element-of-set? didn't change
