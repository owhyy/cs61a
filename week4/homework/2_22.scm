#lang racket

(define (square x) (* x x))

;; version 1: not swapped
;; this will produce the list in reverse order, because
;; cons is adding elemnts to the beginning of the list, like this
;; (cons 1 '()) -> (cons 2 '(1)) -> (cons 3 '(2 1)) ... 
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

;; version 2: swapped
;; this will not produce the desired result, becuase
;; cons creates pairs, not lists
;; so, when the iterative part gets called,
;; it looks like this:
;; (cons '() 1) -> (cons '(() 1) 2) -> (cons '((() 1) 2) 3) ...
;; basically, it says: create a pair where the first element
;; is a list, and the other element is a number
;; thus creating a long chain of list-number pairs, instead of
;; the desired list

(define (square-list-swapped items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))

;; the solution to this is to make sure the second element (the 1, the 2, the 3 ...)
;; are also lists, and to append the answer together with them

;; NOTE: order has to be the same answer first, then car thing
(define (square-list-correct items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                      (list (square (car things)))))))
  (iter items null))

;; alternative and better solution to reverse ;)
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