#lang racket
;(define (f n)
;  (if (< n 3) n
;      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f n)
  (define (iter i a b c)
    (cond
      ((< n 3) n)
      ((<= i 0) a)
      (else (iter (- i 1) (+ a (* b 2) (* c 3)) a b))))
  (iter (- n 2) 2 1 0))