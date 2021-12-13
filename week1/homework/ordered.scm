#lang racket
(require berkeley)

;; this is not an optimal solution from a performance point of view, as it checks against n-i times for every element i
;; (for 1, will check 1 2, 1 3, 1 4 and 1 5) when it only has to check for 1 2, and go to 2, but I don't know how to im-
;; -plement this without checking the internet, and am too tired today to do so ;-)

;; this checks if first is less than every single bf:
;; ex: (1 2 3 4 5)
;; first is 1, so checks if 1 <= 2 -> checks 1 <= 3 -> checks 1 <= 4 -> checks 1 <= 5;

(define (less-first-bf f b)
  (cond ((empty? b) #t)
        ((<= f (first b))
         (less-first-bf f (bf b)))
        (else #f)))

;; this calls less-first-bf on every single elemnt of the sentence:
;; ex: (1 2 3 4 5)
;; first will call 1 (1<=2, 1<=3 ...) then 2 (2<=3, 2<=4 ...) then 3 (3<=4, 3<=5), 4 ...
;; empty means we haven't met a greater-then element, so it's safe to assume all previous
;; elements are <= each other, therefore the sentence is ordered in ascending order
(define (ordered? sent)
  (if (empty? sent) #t
      (and (less-first-bf (first sent) (bf sent))
           (ordered? (bf sent)))))

(ordered? '(2 3 4 5))