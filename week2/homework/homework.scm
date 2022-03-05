#lang racket
(require berkeley)

;; this is just a dump of all the homework excersises for week 2
;; it most likely won't work, as quite a few things are redefined and stuff

;; 1.31
(define (identity a) a)
(define (inc a) (+ 1 a))

(define (product-it term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(factorial 5)

(define (square x) (* x x))
(define (pi-term n) (* (/ (- n 1.0) n) (/ (+ n 1.0) n)))

; my version
(define (pi-approx n)
  (* 4.0 (/ (/ (square (product identity 2 (λ (x) (+ x 2)) n)) (* 2 n))
            (square (product identity 3 (λ (x) (+ x 2)) n)))))

; 3.143163842419198 for n=1000; 0.00000000000003 more than below

; essentially same thing, but written in a different way
;(define (pi-approx n)
;  (* 4.0 (product pi-term 3 (λ (x) (+ x 2)) n)))

;3.143163842419195 for n = 1000

;; 1.32
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


;; 1.33
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

;; 1.40
(define tolerance 0.000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))
(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cube n) (* n n n))
(define (square n) (* n n))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 1 2 3) 1)

;; 1.41
(define (double f)
  (lambda (x) (f (f x))))

(define (inc n) (+ n 1))

(((double (double double)) inc) 5)
;; (double (double double inc)) becomes
;; (double (double (double (double inc))))
;; which is (double (double (double (inc (inc 5))))) ->
;; (double (double (inc (inc (inc (inc 5))))))
;; (double (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))))))))
;; or, in other words, (+ 16 5) -> 21

;; 1.43
(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f x)
  (define (loop n f-comp)
    (if (= n 1) f-comp
      (loop (- n 1) (compose f f-comp))))
  (loop x f))

;; 1.46
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

;; every
(define (every f xs)
  (if (empty? xs) empty
    (cons (f (first xs)) (every f (bf xs)))))

;; keep
(define (keep pred xs)
  (if (empty? xs) empty
    (if (pred (first xs)) (cons (first xs) (keep pred (bf xs)))
      (keep pred (bf xs)))))

(every (lambda (letter) (word letter letter)) 'purple)
(every (lambda (number) (if (even? number) (word number number) number))
       '(781 5 76 909 24))
(keep even? '(781 5 76 909 24))
(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)
(keep (lambda (letter) (member? letter 'aeiou)) 'syzgyzgy)
;(keep (lambda (letter) (member? letter 'aeiou)) '(purple syzgyzgy))
(keep (lambda (wd) (member? 'e wd)) '(purple syzgyzgy))

;; extra
(((lambda (f) (f f)) (lambda (r) (lambda (n) (if (= n 0) 1 (* n ((r r) (- n 1))))))) 5)
