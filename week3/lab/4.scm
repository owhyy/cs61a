#lang racket
(require berkeley)
(define (type-check fn pred arg)
  (if (pred arg)
      (fn arg)
      #f))
(type-check sqrt number? 'hello)
(type-check sqrt number? 4)
