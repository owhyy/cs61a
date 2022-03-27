#lang racket

(define (accumulate f base sequence)
  (cond ((empty? sequence) base)
        (else (f (car sequence)
                 (accumulate f base (cdr sequence))))))

;; y is higher-terms, x is this-coeff, x is unknown
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

;; 79
(horner-eval 2 (list 1 3 0 5 0 1))