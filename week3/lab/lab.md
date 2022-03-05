# Week 3 Lab

1.
```scheme
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
```
* 1st way of changing the order: changing the meaning of `first-denomination` (reversing the order: 1 means 50, 2 means 25 ...)
* 2nd way of changing the order: calling `(cc (- amount (+ 50 (- (first-denomination 50))`

&nbsp;

2. I'll use the 1st option. Note that we also need to change the amount to be increasing (instead of decreasing) inside the recursive call, as values of coins are now reversed
```scheme
(define (first-denomination-alt kinds-of-coins)
  (cond ((= kinds-of-coins 1) 50)
        ((= kinds-of-coins 2) 25)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 5)
        ((= kinds-of-coins 5) 1)))
```
As expected, reversing the order of the coins reverses the order of complexity. Therefore, the book version of `(cc 5 2)` runs in O(n^2), the reversed version will run in O(n^4).

3. Nothing too hard to understand here.

```scheme
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
        ((= kinds-of-coins 5) 50)))
```
&nbsp;

4. Simple type-cheker
```scheme
(define (type-check fn pred arg)
  (if (pred arg)
      (fn arg)
      #f))
```
&nbsp;

5. Creating a type-checked procedure
```scheme
(define (type-check fn pred arg)
  (if (pred arg)
      (fn arg)
      #f))
(define (make-safe fn pred)
  (lambda (x) (type-check fn pred x)))
```

