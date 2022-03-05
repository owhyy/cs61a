#lang sicp
(#%require (lib "27.ss" "srfi"))
(define (square x) (* x x)) 

(define (next n)
  (if (= n 2) 3 (+ n 2)))

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

(define (expmod-2 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (* (expmod base (/ exp 2) m)
             (expmod base (/ exp 2) m))
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
  (try-it (+ 1 (random-integer (- n 1)))))

;; if a%n=a^n%n, then a is probably prime
;; here we pick a random a < n

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (smallest-divisor n) 
  (find-divisor n 2)) 
  
(define (find-divisor n test-divisor) 
  (cond ((> (square test-divisor) n) n) 
        ((divides? test-divisor n) test-divisor) 
        (else (find-divisor n (next test-divisor))))) 
  
(define (divides? a b) 
  (= (remainder b a) 0)) 

(define (timed-prime-test n) 
  (start-prime-test n (runtime))) 
  
(define (start-prime-test n start-time) 
  (if (fast-prime? n 100) 
      (report-prime n (- (runtime) start-time)))) 
  
(define (report-prime n elapsed-time) 
  (newline) 
  (display n) 
  (display " *** ") 
  (display elapsed-time)) 
  
(define (search-for-primes lower upper) 
  (define (iter n) 
    (cond ((<= n upper) (timed-prime-test n) (iter (+ n 2))))) 
  (iter (if (odd? lower) lower (+ lower 1)))) 
  
(search-for-primes 1000 1019)       ; 1e3 
(search-for-primes 10000 10037)     ; 1e4 
(search-for-primes 100000 100043)   ; 1e5 
(search-for-primes 1000000 1000037) ; 1e6 
(search-for-primes 1000000000 1000000021)              ; 1e9 
(search-for-primes 10000000000 10000000061)            ; 1e10 
(search-for-primes 100000000000 100000000057)          ; 1e11 
(search-for-primes 1000000000000 1000000000063)        ; 1e12

;; although not visible at small numbers (for the above cases, my procedure shows approximately the same results)
;; however, the new version computes the recursion twice, when calling multiplication