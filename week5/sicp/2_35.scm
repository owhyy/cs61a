#lang racket
;; 2.35

(define (accumulate f base sequence)
  (cond ((empty? sequence) base)
        (else (f (car sequence) 
                 (accumulate f base (cdr sequence))))))

(define (count-leaves-book t)
  (cond ((empty? t) 0)
        ((not (pair? t)) 1)
        (else (+ (count-leaves-book (car t))
                 (count-leaves-book (cdr t))))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (leaf) (if (not (pair? leaf)) 1 (count-leaves leaf))) t)))