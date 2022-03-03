# Week 1 Homework
1. ???
2. Simple procedure, takes a sentence and returns a sentence consisting of the squares of the elements.

```
(define (square x) (* x x))

(define (squares sent)
  (if (empty? sent) empty
      (se (square (first sent))
          (squares (bf sent)))))
```scheme

3. The procedure consists of 3 parts: `switch-aux`, which takes a sentence as argument and replaces all occurences of **I** or **me** with **you**, and every occurence of **you ** with **me**. The second part is the `add-i-to-beginning`, which takes a sentence and returns a sentence in which, if the first word is **me**, it will be replaced with **I**. The last part - `switch` joins the two together.

```
(define (switch-aux sent)
  (cond ((empty? sent) empty)
        ((member? (first sent) '(I me i Me ME))
         (se 'you (switch-aux (bf sent))))
        ((member? (first sent) '(You you YOU))
         (se 'me (switch-aux (bf sent))))
        (else (se (first sent) (switch-aux (bf sent))))))

(define (add-i-to-beginning sent)
  (if (equal? (first sent) 'me)
      (se 'I (bf sent))
      sent))

(define (switch sent)
  (add-i-to-beginning (switch-aux sent)))
```scheme

4. This is not an efficient solution, because it won't stop when it reached a number that is less than the one before.

The way this works is, it will continously check the first element of the list to all the others, and will do this untill it runs out of elements to check. This only happens when there is no element less than the ones before it, since otherwise the `less-first-bf` would return `#false`. However, it is possible to do the same thing, but not check everything before the current element, since if we have a case like `(1 2 3 1 4)`, reaching the case where we check `(<= 3 1`) would be `#false`.

Ex: In my implementation, for a sentence like `(1 2 3 4 5)`, it will check `(<= 1 2)`, `(<= 1 3)`, `(<= 1 4)` and `(<= 1 5)`.

A good implementation would be to only check `(<= 1 2)` and advance forward, checking `(<= 2 3)` but I haven't found a way and I'm writing the markdown version while in week 4, so I don't quite feel like going back and completing it.

```
(define (less-first-bf f b)
  (cond ((empty? b) #t)
        ((<= f (first b))
         (less-first-bf f (bf b)))
        (else #f)))

(define (ordered? sent)
  (if (empty? sent) #t
      (and (less-first-bf (first sent) (bf sent))
           (ordered? (bf sent)))))
```scheme

5.
