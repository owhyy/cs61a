#lang racket
(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((or (< amount 0) (= kinds-of-coins 0)) 0)
        ((= amount 0) 1)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; how would swapping the first and second of the
;; base cases (questions in the cond) will affect the program

;; the results will differ only when calling (cc 0 0).
;; however, this never happens unless called specifically (no recursion ever gets to (cc 0 0))
;; thus, the set of arguments for which the original cc would differ from this one is {0}.