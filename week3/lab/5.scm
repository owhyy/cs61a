#lang racket
(require berkeley)
(define (type-check fn pred arg)
  (if (pred arg)
      (fn arg)
      #f))
(define (make-safe fn pred)
  (lambda (x) (type-check fn pred x)))