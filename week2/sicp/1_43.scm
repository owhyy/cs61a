#lang racket
(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f x)
  (define (loop n f-comp)
    (if (= n 1) f-comp
        (loop (- n 1) (compose f f-comp))))
  (loop x f))