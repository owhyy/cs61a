#lang racket

(define beside car)
(define below car)

(define (split painter-operation smaller-operation)
  (define (loop p x)
    (if (= x 0)
        p
        (let ((smaller (loop p (- x 1))))
          (painter-operation p (smaller-operation smaller smaller)))))
  (lambda (painter n) (loop painter n)))