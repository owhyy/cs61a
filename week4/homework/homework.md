# Week 4 Homework

## *SICP* Excersises
### Excersise 2.17
```scheme
(define (last-pair l)
  (let ((next-pair (if (empty? l) empty (last-pair (cdr l)))))
    (if (empty? next-pair)
        l
        (last-pair (cdr l)))))
```
### Excersise 2.20
```scheme
(define (even? x)
  (= (remainder x 2) 0))

(define (same-parity f . r)
  (define (handle-even r)
    (cond ((null? r) null)
          ((even? (car r)) (cons (car r) (handle-even (cdr r))))
          (else (handle-even (cdr r)))))
  (define (handle-odd r)
    (cond ((null? r) null)
          ((not (even? (car r))) (cons (car r) (handle-odd (cdr r))))
          (else (handle-odd (cdr r)))))
  (cons f (if (even? f)
              (handle-even r)
              (handle-odd r))))

(define (test f . r)
  (if (even? f) 10 (if (null? r) null (test (cdr r)))))
```
### Excersise 2.22

**Version 1**: not swapped.\
This will produce the list in reverse order, because `cons` is adding elements to the beginning of the list, like this:\
`(cons 1 '()) -> (cons 2 '(1)) -> (cons 3 '(2 1))` ...

```scheme
(define (square x) (* x x))
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))
```
&nbsp;

**Version 2**: swapped.\
This will not produce the desired result, becuase `cons` creates pairs, not lists, so, when the iterative part gets called, it looks like this:
`(cons '() 1) -> (cons '(() 1) 2) -> (cons '((() 1) 2) 3) ...`

Basically, it says: create a pair where the first element is a list, and the other element is a number. This will create a long chain of list-number pairs, instead of the desired list

```scheme
(define (square x) (* x x))
(define (square-list-swapped items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))
```

The solution to this is to make sure the second element (the 1, 2, 3 ...) are also lists, and to append the answer together with them

```scheme
(define (square-list-correct items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                      (list (square (car things)))))))
  (iter items null))
```

&nbsp;

Cool little thing you might've noticed: iterative implementations process lists in reverse order. So a even better implementation to `reverse` (from the lab) is this:
```scheme
(define (reverse items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (car things)
                    answer))))
  (iter items null))

(define (square-list-map items)
  (map square items))
```
### Excersise 2.23

Doing it like this reverses it
```scheme
(define (for-eachq f loe)
  (cond ((empty? loe) empty)
        (else (for-each f (cdr loe))
              (f (car loe)))))
```
Doing it like this adds the empty to the result
```scheme
(define (for-each f loe)
  (cond ((empty? loe) #t)
        (else (and (f (car loe))
                   (for-each f (cdr loe))))))
```

&nbsp;

## Course-specific excersises
* `substitute`
```scheme
(define (substitute-word s old new)
  (cond ((empty? s) empty)
        ((equal? (first s) old) (se new (substitute (bf s) old new)))
        (else (se (first s) (substitute (bf s) old new)))))

(define (substitute l old new)
  (cond ((empty? l) empty)
        ((sentence? l) (substitute-word l old new))
        (else (cons (substitute (car l) old new)
                    (substitute (cdr l) old new)))))
```
`substitute-word` replaces the old word with the new one (similar to a excersise from the previous weeks)
`substitute` will call `substitute-word` on every word of the sentence

&nbsp;
* `substitute2`

```scheme
(define (substitute-word2 s old new)
    (cond ((empty? s) empty)
          ((empty? old) (se (first s) (substitute-word2 (bf s) old new)))
          ((equal? (first s) (first old)) (se (first new) (substitute-word2 (bf s) old new)))
          (else (substitute-word2 s (bf old) (bf new)))))

(define (substitute2 l old new)
  (cond ((empty? l) empty)
        ((sentence? l) (substitute-word2 l old new))
        (else (cons (substitute2 (car l) old new)
                    (substitute2 (cdr l) old new)))))
```
Quite the same thing as `substitute`, the only difference being that in `substitute-word2` we recurse on all of the sentences that can have sentences inside them

