#lang racket

(define (double n) (+ n n))
(define (half n) (/ n 2))
(define (* x n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (double b) (half n)))
          (else (iter (+ a b) b (- n 1)))))
  (iter 0 x n))