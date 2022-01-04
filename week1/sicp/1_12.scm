#lang racket
(define (pascal r c)
  (if (or (= r c) (= c 1))
      1
      (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c))))