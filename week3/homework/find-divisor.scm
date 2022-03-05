#lang racket

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

;; O(sqrt (n)) time
;; checks if n % test-divisor = 0, produces test-divisor
;; otherwise check again with 1 + test-divisor
;; untill test-divisor > sqrt(n))

(define (square x) (* x x))
(define (divides? a b) (= (remainder b a) 0))

(define (smallest-divisor n) (find-divisor n 2))