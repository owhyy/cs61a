#lang racket
(require berkeley)

;; perf number: sum of all factors less than itself
(define (sum-of-factors n c res)
    (cond ((= c n) res)
          ((= (remainder n c) 0) (sum-of-factors n (+ c 1) (+ res c)))
          (else (sum-of-factors n (+ c 1) res))))
(define (next-perf n)
  (let ((res (sum-of-factors n 1 0)))
    (if (= res n) res (next-perf (+ n 1)))))

(next-perf 1)
(next-perf 7)
(next-perf 29)