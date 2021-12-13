#lang racket
(require berkeley)

(define (dupls-removed sent)
  (cond
    ((empty? sent) empty)
    ((member? (first sent) (butfirst sent))
     (dupls-removed (butfirst sent)))
    (else (sentence (first sent) (dupls-removed (butfirst sent))))))
