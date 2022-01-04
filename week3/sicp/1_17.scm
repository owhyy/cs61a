#lang racket
(define (double n) (+ n n))
(define (half n) (/ n 2))
(define (* n x)
  (cond ((= n 0) 0)
        ((even? n) (double (* x (half n))))
        (else (+ x (* x (- n 1))))))