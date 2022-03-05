#lang sicp
(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
           (nontrivial-root (expmod base (/ exp 2) m) m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (nontrivial-root n m)
  (let ((root (remainder (square n) m)))
    (if (and (not (= n 1)) (not (= n (- m 1))) (= root 1))
        0
        root)))

(define (miller-rabin-test n a) (= (expmod a (- n 1) n) 1))

(define (prime? n)
  (cond ((= n 2) #t)
        ((even? n) #f)
        (else (prime-helper n (- n 1)))))

(define (prime-helper n a)
  (cond ((= a 0) #t)
        ((miller-rabin-test n a) (miller-rabin-test n (- a 1)))
        (else #f)))