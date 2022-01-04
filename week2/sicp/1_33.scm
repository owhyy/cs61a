#lang racket
(define (accumulate-filter satisf? combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((satisf? a) (combiner (term a) (accumulate-filter satisf? combiner null-value term (next a) next b)))
        (else (accumulate-filter satisf? combiner null-value term (next a) next b))))

(define (accumulate-filter-it satisf? combiner null-value term a next b)
  (define (iter result a b)
    (cond ((> a b) result)
          ((satisf? a) (iter (combiner (term a) result) (next a) b))
          (else (iter result (next a) b))))
  (iter null-value a b))

(define (prime? n)
  (define (loop n i)
    (cond ((> i (sqrt n)) #t)
          ((= (remainder n i) 0) #f)
          (else (loop n (+ i 1)))))
  (loop n 2))

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))
  
(define (square n)
  (* n n))

(define (sum-sq-prime-numbers a b)
  (accumulate-filter-it prime? + 0 square a (λ (x) (+ x 1)) b))

(define (prod-coprime n)
  (accumulate-filter-it (λ (x) (= (gcd x n) 1)) * 1 (λ (x) x) 0 (λ (x) (+ x 1)) n))
