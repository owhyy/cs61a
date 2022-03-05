# Week 2 Lab

1.
```scheme
(lambda (x) (+ x 3)) -> a procedure
((lambda (x) (+ x 3)) 7) -> applies the lambda to 10

(define (make-adder num)
  (lambda (x) (+ x num))) -> a procedure
((make-adder 3) 7) -> makes a lambda adding 3 to something, applies it to 7, thus 10

(define plus3 (make-adder 3)) -> a procedure
(plus3 7) -> applies the add 3 to somehing procedure to 7, thus 10

(define (square x) (* x x)) -> a procedure
(square 5) -> 25
(define sq (lambda (x) (* x x))) -> a procedure
(sq 5) -> 25
(define (try f) (f 3 5)) -> a procedure
(try +) -> applies + to 3 and 5, thus 8
(try word) -> applies word to 3 5 and 5, thus '35
```

2. I'm pretty sure I did the rest of the lab, but I can't find it anywhere. Oh well...
