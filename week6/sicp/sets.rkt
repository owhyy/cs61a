#lang racket

;; set representation
;; every element in a set appears only once

;; checks if element is in set
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; adds element to set if it isn't already there
(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

;; intersection of 2 sets
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


