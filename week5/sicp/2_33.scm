#lang racket

(define (square x) (* x x))

;; accumulate
(define (accumulate f base sequence)
  (cond ((empty? sequence) base)
        (else (f (car sequence)
                 (accumulate f base (cdr sequence))))))

;; map
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) empty sequence))

(map square (list 1 2 3 4))

;; append
(define (append seq1 seq2)
 (accumulate cons seq1 seq2))

(append (list 1 2) (list 3 4))

;; length
(define (length seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))

(length (list (list 1 2) (list 3 4)))