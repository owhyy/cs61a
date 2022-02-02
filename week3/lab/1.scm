#lang racket
(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        (( or (< amount 0) (> kinds-of-coins 5)) 0)
        (else (+ (cc amount
                     (+ kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination-alt
                         kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; 1st way of changing the order: changing the meaning of first-denomination
;; 2nd way of changing the order: calling (cc (- amount (+ 50 (- (first-denomination 50))

;; for ex. 2, we will use 1st way
;; note that we also need to change the amount to be increasing (instead of decreasing)
;; inside the recursive call, as values of coins are now reversed
(define (first-denomination-alt kinds-of-coins)
  (cond ((= kinds-of-coins 1) 50)
        ((= kinds-of-coins 2) 25)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 5)
        ((= kinds-of-coins 5) 1)))
;; as expected, reversing the order of the coins reverses the order of complexity
;; thus, the normal version of (cc 5 2) runs in O(n^2), the reversed version will run
;; in O(n^4), since a k of 1 runs in O(n^5)
