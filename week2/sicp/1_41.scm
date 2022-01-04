#lang racket
(define (double f)
  (lambda (x) (f (f x))))

(define (inc n) (+ n 1))


(((double (double double)) inc) 5)
;; (double (double double inc)) becomes
;; (double (double (double (double inc))))
;; which is (double (double (double (inc (inc 5))))) ->
;; (double (double (inc (inc (inc (inc 5))))))
;; (double (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))))))))
;; or, in other words, (+ 16 5) -> 21