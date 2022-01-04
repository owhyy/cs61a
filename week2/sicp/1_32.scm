#lang racket
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-it combiner null-value term a next b)
  (define (iter result a b)
  (if (> a b)
      result
      (iter (combiner (term a) result) (next a) b)))
  (iter null-value a b))

