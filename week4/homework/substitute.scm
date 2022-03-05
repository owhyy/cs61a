#lang racket
(require berkeley)

(define (substitute-word s old new)
  (cond ((empty? s) empty)
        ((equal? (first s) old) (se new (substitute (bf s) old new)))
        (else (se (first s) (substitute (bf s) old new)))))

(define (substitute-word2 s old new)
    (cond ((empty? s) empty)
          ((empty? old) (se (first s) (substitute-word2 (bf s) old new)))
          ((equal? (first s) (first old)) (se (first new) (substitute-word2 (bf s) old new)))
          (else (substitute-word2 s (bf old) (bf new)))))

(define (substitute l old new)
  (cond ((empty? l) empty)
        ((sentence? l) (substitute-word l old new))
        (else (cons (substitute (car l) old new)
                    (substitute (cdr l) old new)))))


(substitute '((guitar lead) (bass guitar) (rhytm guitar) drums)
            'guitar 'axe)
(substitute '(((guitar lead) (bas guitar)) (drums guitar) drums)
            'guitar 'axe)

(define (substitute2 l old new)
  (cond ((empty? l) empty)
        ((sentence? l) (substitute-word2 l old new))
        (else (cons (substitute2 (car l) old new)
                    (substitute2 (cdr l) old new)))))

(substitute2 '((4 calling birds) (3 french hens) (2 turtle doves))
             '(1 2 3 4) '(one two three four))