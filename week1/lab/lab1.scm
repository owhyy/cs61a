#lang racket
(require berkeley)

;; piglatin
(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

;; plural
(define (plural w)
  (if (equal? (last w) 'y)
      (if (vowel? (last (butlast w))) (word w 's)
          (word (butlast w) 'ies))
          (word w 's)))

(define (vowel? c)
  (member? c '(a e i o u)))

;; biggest
(define (biggest n1 n2)
  (if (> n1 n2) n1 n2))

(define (sum-squares-biggies n1 n2 n3)
  (+ (square (biggest (biggest n1 n2) n3))
     (square (cond ((not (= (biggest n1 n2) (biggest (biggest n1 n2) n3))) (biggest n1 n2))
           ((not (= (biggest n1 n3) (biggest (biggest n1 n2) n3))) (biggest n1 n3))
           (else (biggest n2 n3))))))

;; dupls-removed
(define (dupls-removed sent)
  (cond
    ((empty? sent) empty)
    ((member? (first sent) (butfirst sent))
     (dupls-removed (butfirst sent)))
    (else (sentence (first sent) (dupls-removed (butfirst sent))))))


