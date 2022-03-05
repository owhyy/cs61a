# Week 3 Homework

1. *SICP* excersises. Check the `.scm` files. I don't really remember a lot, so I haven't included them in this file.
&nbsp;

2. Straightforward implementation: `sum-of-factors` produces the sum of factors of a number (duh), `next-perf` will increment numbers 1 by 1, untill it reaches a number whose sum of factors is itself.
  ```scheme
  (define (sum-of-factors n c res)
   (cond ((= c n) res)
    ((= (remainder n c) 0) (sum-of-factors n (+ c 1) (+ res c)))
    (else (sum-of-factors n (+ c 1) res))))

  (define (next-perf n)
   (let ((res (sum-of-factors n 1 0)))
    (if (= res n) res (next-perf (+ n 1)))))
  ```

  &nbsp;

  3.
  ```scheme
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

  ```
The results will differ only when calling (cc 0 0). However, this never happens unless called specifically (no recursion ever gets to (cc 0 0))
  Thus, the set of arguments for which the original cc would differ from this one is {0}.

  &nbsp;

  4. The sum of the product of `b` and `product` and the difference of `n` and `counter` in their initial states is always equal to `b`
  *(b * product) + (n - counter) = b*

  5. Extra for experts

  a. Number of partitions of a positive integer

  ```scheme
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
```

b. Counting partitions is like making change, where the coins are valued at their faced value

c. Didn't do it
