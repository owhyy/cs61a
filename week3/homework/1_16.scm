#lang racket
(define (fast-exp x n)
  (define (iter N Y Z)
    (cond ((= N 0) Y)
          ((even? N) (iter (truncate (/ N 2)) Y (* Z Z)))
          (else (iter (truncate (/ N 2)) (* Y Z) (* Z Z)))))
  (iter n 1 x))

(define (fast-exp2 x n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (* b b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 x n))
          