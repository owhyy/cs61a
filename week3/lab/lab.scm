#lang racket
(require berkeley)

;; reversing the order
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

(define (first-denomination-alt kinds-of-coins)
  (cond ((= kinds-of-coins 1) 50)
        ((= kinds-of-coins 2) 25)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 5)
        ((= kinds-of-coins 5) 1)))

;; cc taking kinds-of-coins sentence
(define (count-change amount) (cc amount '(50 25 10 5 1)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (empty? kinds-of-coins)) 0)
        (else (+ (cc amount
                     (bf kinds-of-coins))
                 (cc (- amount
                         (first kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))#lang racket

;; type-check
(define (type-check fn pred arg)
  (if (pred arg)
      (fn arg)
      #f))

(type-check sqrt number? 'hello)
(type-check sqrt number? 4)

;; make-safe
(define (make-safe fn pred)
  (lambda (x) (type-check fn pred x)))

