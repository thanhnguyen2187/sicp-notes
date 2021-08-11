; Exercise ?.?

#lang sicp

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree
                               factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree-v2 tree
                       factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-v2 sub-tree factor)
             (* sub-tree factor)))
       tree))

(define TR (list 1 (list 2 3 4) (list 4 (list 5 6 7))))
(scale-tree TR 3)
(scale-tree-v2 TR 3)
