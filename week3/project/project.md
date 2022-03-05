# Project 1

1. `best-total`
```scheme
(define (best-total hand)
  (define (loop hand sum-so-far)
    (cond ((empty? hand) sum-so-far)
          ((member? (bl (first hand)) '(A a))
           (let ((new-sum (+ 11 (loop (bf hand) sum-so-far))))
             (if (> new-sum 21) (- new-sum 10) new-sum)))
          ((member? (bl (first hand)) '(K Q J k q j))
           (loop (bf hand) (+ sum-so-far 10)))
          (else (loop (bf hand) (+ sum-so-far (butlast (first hand)))))))
  (loop hand 0))
```

This will first add 11 if the card is an ace card, then compute the values of all other cards. If that is > 21, it will remove 10 from it, otherwise it's just going to produce the total with the 11.

2. `stop-at-17`
```scheme
(define (stop-at-17 hand dealer-card)
  (< (best-total hand) 17))
```

3. `play-n`
```scheme
(define (play-n strategy n)
  (define (play win lose i)
    (if (> i n) (- win lose)
        (cond ((= (twenty-one strategy) 1) (play (+ win 1) lose (+ i 1)))
              ((= (twenty-one strategy) -1) (play win (+ lose 1) (+ i 1)))
              (else (play win lose (+ i 1))))))
  (play 0 0 0))
```

This basically plays n games, if it wins then it will adds 1 to the `win` variable, if it loses, 1 to the `lose`, and at the end it will just find the difference between the two.

4. `dealer-sensitive`
```scheme
(define (dealer-sensitive hand dealer-card)
  (cond ((and (member? (bl dealer-card) '(a k q j 10 7 8 9))
              (< (best-total hand) 17)) #t)
        ((and (member? (bl dealer-card) '(2 3 4 5 7))
              (< (best-total hand) 12)) #t)
        (else #f)))
```
The rest should be self-explanatory if you read the .scm file
