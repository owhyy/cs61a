
# Week 1 First Lab
1.
```scheme
3 -> 3
(+ 2 3) -> 5
(+) -> procedure
(sqrt 16) -> 4
(+ (* 3 4) 5)
+ -> error
'+ -> '+
'hello -> 'hello
'(+ 2 3) -> '(+ 2 3)
'(good morning) -> '(good morning)
(first 274) -> 2
(butfirst 274) -> 74
(first 'hello) -> 'h
(first hello) -> error
(+ (first 23) (last 45)) -> 7
(define pi 3.14159) -> p1
pi -> 3.14159
'pi -> 'pi
(+ pi 7) -> 10.14159
(* pi pi) -> 9.869586
(define (square x) (* x x)) -> square
(square 5) -> 25
(square (+ 2 3)) -> 25
```
&nbsp;

2. The code is, I believe, in the lecture.
```scheme
(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))
```
&nbsp;

# Week 1 Second Lab
1.
```scheme
(define a 3) -> a
(define b (+ a 1)) -> b
(+ a b (* a b)) -> 19
(= a b) -> #f
(if (and (> b a) (< b (* a b)))
    b
    a) -> 4
(cond ((- a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) -> 17

(+ 2 (if (> b a) b a)) -> 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
    (+ a 1)) -> 16
((if (< a b) + -) a b) -> 7
```
&nbsp;

2. This checks if the last letter of a word is *y*, in which case it triggers the next check, that checks if the word before it is a vowel, in which case it appends a `'s`, otherwise it appends a `'ies`. If the last word isn't a *y*, it just appends `'s`, as there are no special rules.
```scheme
(define (plural w)
  (if (equal? (last w) 'y)
      (if (vowel? (last (butlast w)))
          (word w 's)
          (word (butlast w) 'ies))
       (word w 's)))

(define (vowel? c)
  (member? c '(a e i o u)))
```
&nbsp;

3. This takes 3 numbers and adds the squares of the biggest 2 of them. It does so by finding the maximum of the three, and then finding the next maximum that is not the same as the other one. I presume this will work if there are 2 identical maximums, but I haven't tested it out.
```scheme
(define (biggest n1 n2)
  (if (> n1 n2) n1 n2))

(define (sum-squares-biggies n1 n2 n3)
  (+ (square (biggest (biggest n1 n2) n3)) ; finds maximum of the three
     (square (cond ((not (= (biggest n1 n2) (biggest (biggest n1 n2) n3))) (biggest n1 n2))
           ((not (= (biggest n1 n3) (biggest (biggest n1 n2) n3))) (biggest n1 n3))
           (else (biggest n2 n3))))))
```
