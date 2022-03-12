#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define m1 (make-mobile (make-branch 10 10) (make-branch 3 5)))
(define m2 (make-mobile (make-branch 10 10) (make-branch 3 m1)))
(define m3 (make-mobile (make-branch 15 15) (make-branch 15 15)))

(define lm1 (left-branch m1))
(define rm1 (right-branch m1))
(branch-length lm1)
(branch-length rm1)
(branch-structure lm1)
(branch-structure rm1)

;; b
(define (total-weight mobile)
  (cond ((empty? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

(total-weight m1)
(total-weight m2)

;; c
(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (cond ((not (pair? mobile)) #t)
        (else (and
               (= (torque (left-branch mobile)) (torque (right-branch mobile)))
               (balanced? (branch-structure (left-branch mobile)))
               (balanced? (branch-structure (right-branch mobile)))))))
(balanced? m3)

