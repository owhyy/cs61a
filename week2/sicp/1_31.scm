#lang racket
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