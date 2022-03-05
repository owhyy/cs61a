# Homework week 2

1.
## Excersise 1.31


## Excersise 1.32

## Excersise 1.33

## Excersise 1.40
## Excersise 1.41

## Excersise 1.43
## Excersise 1.46

2. Should be pretty self-explanatory
```scheme
(define (every f xs)
  (if (empty? xs) empty
      (cons (f (first xs)) (every f (bf xs)))))
```

3.
```
(define (keep pred xs)
  (if (empty? xs) empty
      (if (pred (first xs)) (cons (first xs) (keep pred (bf xs)))
            (keep pred (bf xs)))))
```
4. I don't understand how this works. The text writen below is useless.
Basically, we need a way to make our factorial be a recursive lambda function. We can do this using composition of functions.
  1. First part: `(f f)`; this calls `f(f(x))` (the same way combining `f` and `g` calls `f(g(x))`
  2. the part that actually does the work `(n)` is wrapped inside `r`, because `r` is actually the recursive part, which calls itself untill `n` is 0
```scheme
(((lambda (f) (f f)) ; calls itself on itself, like a infinite loop kind of thing?
  (lambda (r) (lambda (n) ; I don't really  know why we need r here
    (if (= n 0)
      1
      (* n ((r r) (- n 1))))))) 5)
```
