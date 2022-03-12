#lang racket
;; 2.27
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

(reverse x)
(deep-reverse x)
