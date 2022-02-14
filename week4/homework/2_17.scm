#lang racket

(define (last-pair l)
  (let ((next-pair (if (empty? l) empty (last-pair (cdr l)))))
    (if (empty? next-pair)
        l
        (last-pair (cdr l)))))

(last-pair (list 23 72 149 34))