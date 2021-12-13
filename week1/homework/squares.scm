#lang racket
(require berkeley)

(define (square x) (* x x))

(define (squares sent)
  (if (empty? sent) empty
      (se (square (first sent))
          (squares (bf sent)))))
