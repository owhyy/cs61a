#lang racket
;; NOTE: unfinished, need to do 3) of extra for experts
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

;; Counting partitions is like making change, where the coins are valued at their faced value

