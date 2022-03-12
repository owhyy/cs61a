#lang racket
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
