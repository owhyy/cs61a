#lang racket
(require berkeley)

(define x '(a (b c) d))
(car x) ;; 'a is the first element of the sentence (list) -> its what get returned by car
(cdr x) ;; '((b c) d) gets represented as a sentence
(car (cdr (cdr x)))
(car (cdr x))