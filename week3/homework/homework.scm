#lang racket
(require berkeley)

;; next-perf
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

;; partitions
(define (number-of-partitions amount) (cp-iter amount amount))
(define (cp-iter amount kinds-of-pieces)
  (define (iter amount kinds-of-pieces result)
    (cond ((or (< amount 0) (= kinds-of-pieces 0)) result)
          ((= amount 0) (iter amount kinds-of-pieces (+ result 1)))
          (else (iter amount
                      (- kinds-of-pieces 1)

  (iter amount kinds-of-pieces 0))

(define (cp amount kinds-of-pieces)
  (cond ((or (< amount 0) (= kinds-of-pieces 0)) 0)
        ((= amount 0) 1)
        (else (+ (cp amount
                     (- kinds-of-pieces 1))
                 (cp (- amount
                        kinds-of-pieces)
                     kinds-of-pieces)))))

(number-of-partitions 3)
(number-of-partitions 5)
(number-of-partitions 10)
