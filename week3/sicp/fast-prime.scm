#lang racket

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

;; (base^e/2)^2 % m if exp is even
;; base^e-1 % m if m is odd

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; if a%n=a^n%n, then a is probably prime
;; here we pick a random a < n

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; checks if n is prime, using fermat-test: if it's true, then
;; most likely it's a prime number (but it's not guaranteed), so
;; check again with another random number