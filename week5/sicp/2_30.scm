#lang racket

(define (square-tree-no-map t)
  (cond ((empty? t) empty)
        ((not (pair? t)) (* t t))
        (else (cons (square-tree-no-map (car t))
                    (square-tree-no-map (cdr t))))))

(define (square-tree-map t)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (* sub-tree sub-tree))) t))
             

(define tree1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree-no-map tree1)
(square-tree-map tree1)