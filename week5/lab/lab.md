# Week 5 Lab
1.
```scheme
;; 2.25
(define l1 '(1 3 (5 7) 9))
(car (cdr (car (cdr (cdr l1)))))

(define l2 '((7)))
(car (car l2))

(define l3 '(1 (2 (3 (4 (5  (6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))

;; 2.53
(list 'a 'b 'c) ; -> '(a b c)
(list (list 'george)) ; -> ('(george)) correct: '((george))
;; wrong: the ' goes all the way up, so it's like a sentence of lists
(cdr '((x1 x2) (y1 y2))) ; -> '((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; -> '(y1 y2)
(pair? (car '(a short list))) ; -> will return 'a; it's not a pair, so #f
(memq 'red '((red shoes) (blue socks))) ; -> #f
(memq 'red '(red shoes blue socks)) ; -> '(red shoes blue socks)
```

&nbsp;

2.
```scheme
(car ''abracadabra)
```
This is essentially saying "create a pair, with the words `'` and `'abracadabra` so `(car (cons ' 'abracadabra))` will return the quote

&nbsp;

3.
```scheme
(define x (list (list 1 2) (list 3 4)))

(define (reverse l)
  (define (loop res rol)
    (if (empty? rol)
        res
        (loop (cons (car rol) res)
              (cdr rol))))
  (loop empty l))

(define (deep-reverse lol)
   (if (list? (car lol)) (map (lambda (element) (deep-reverse element))
                       (reverse lol))
      (reverse lol)))
```

I'll use the list provided in the excersise, that is `'((1 2) (3 4))`:
1. Checks if `(car '((1 2) (3 4))` is a list. This returns `#t`(as `'(1 2)` is obviously a list), so this will call `deep-reverse` of every element of the reversed deeplist. In other words, this will call `deep-reverse` on every sublist of the deep-list `'((3 4) (1 2))`. Just to be clear, by sublist I mean the lists `'(3 4)` and `'(1 2)`.
2. The lambda first calls `(deep-reverse '(3 4))`, which checks `(list? (car '(3 4))` which will return `#f`, since `3` is not a list. What this will do is call reverse on the list `'(3 4)`, which will result in `'(4 3)`. The `map` takes care of `cons`-ing everything together the base cases, so the result being produced happens without us having to think about it.
3. Then the lambda will be called on the second sublist, which is `'(1 2)`, which will run identical to the 2nd step, calling `reverse` on the list, which will result in the list `'(2 1)`.
4. Now `map` reaches the base-case (no more elements in the deeplist), and our sublists look like `'(4 3)` and `'(2 1)`, which is the answer we want (for the sublists). As I said, `map` takes care of `cons`-ing everything together, so our result will be the deeplist `'((4 3) (2 1))`, which is the answer we were looking for.

