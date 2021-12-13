#lang racket
(require berkeley)
(define (plural w)
  (if (equal? (last w) 'y)
      (if (vowel? (last (butlast w))) (word w 's)
      (word (butlast w) 'ies))
      (word w 's)))

(define (vowel? c)
  (member? c '(a e i o u)))