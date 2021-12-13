#lang racket
(require berkeley)
(define (biggest n1 n2)
  (if (> n1 n2) n1 n2))

(define (sum-squares-biggies n1 n2 n3)
  (+ (square (biggest (biggest n1 n2) n3))
     (square (cond ((not (= (biggest n1 n2) (biggest (biggest n1 n2) n3))) (biggest n1 n2))
           ((not (= (biggest n1 n3) (biggest (biggest n1 n2) n3))) (biggest n1 n3))
           (else (biggest n2 n3))))))