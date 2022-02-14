#lang racket

(define (cons x y)
  (lambda (m) (m x y)))

;; the lambda acts as the binding agent
;; it creates a procedure that applies m to x and y, although, in reality, it doesn't apply anything

(define (car z)
  (z (lambda (p q) p)))

;; here z is the cons function, which will get applied to (lambda (p q) p)
;; something like (cons 3 5) will result in ((cons 3 5) (lambda (p q) p))
;; (cons 3 5) will get applied to the lambda taking exactly 2 arguments (3 as p, 5 as q)
;; and the lambda will return p -> 3

(define (cdr z)
  (z (lambda (p q) q)))

;; same thing, except it will return q -> 5