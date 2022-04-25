#lang racket

;; huffman
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;; a tree is a (list left-branches, right-branches their symbols and total weight))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree) ; if the node is a leaf
    (list (symbol-leaf tree)) ; then create a list of its symbol
    (caddr tree))) ; otherwise get all the symbols of the branch

(define (weight tree)
  (if (leaf? tree) ; if the node is a leaf
    (weight-leaf tree) ; get its weight
    (cadddr tree))) ; otherwise get total weight of branch

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      null
      (let ((next-branch
              (choose-branch (car bits) current-branch))) ; determines whether we should go left or right
        (if (leaf? next-branch) ; if we reach a leaf
          (cons (symbol-leaf next-branch) ; get its symbol
                (decode-1 (cdr bits) tree)) ; remove it from the list
          (decode-1 (cdr bits) next-branch))))) ; go deeper
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    null
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)
                             (cadr pair))
                  (make-leaf-set (cdr pairs))))))
;; ---

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 1 0))
(decode sample-message sample-tree)
