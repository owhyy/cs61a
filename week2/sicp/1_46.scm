#lang racket
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess) guess
        ((iterative-improve good-enough? improve) (improve guess)))))

(define tolerance 0.00001)
(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f guess)
  ((iterative-improve (lambda (x) (close-enough? x (f x))) f) guess))

(define (square n) (* n n))

(define (sqrt n)
  ((iterative-improve (lambda (x) (< (abs (- (square x) n)) tolerance))
                      (lambda (x) (average x (/ n x)))) 1.0))