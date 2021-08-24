; Exercise 2.63
;
; Each of the following two procedures converts to a binary tree to a list.
;
; ...
;
; a. Do the two procedures produce the same result for every tree? If not, how
; do the results differ? What lists do the two procedures produce for the trees
; in Figure 2.16?
;
; b. Do the two procedures have the same order of growth in the number of steps
; required to convert a balanced tree with /n/ elements into a list? If not,
; which one grows more slowly?
;
; ---
;
; The second one grows more slowly, since the process is iterative.

#lang sicp

(define (entry        tree) (car   tree))
(define (left-branch  tree) (cadr  tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((> x (entry set)) (element-of-set? x (left-branch  set)))
        ((< x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x (list) (list)))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (right-branch set))
                                      (left-branch set)))
        ))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree
                        result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define TR1 (make-tree 7
                       (make-tree 3
                                  (make-tree 1 nil nil)
                                  (make-tree 5 nil nil))
                       (make-tree 9
                                  nil
                                  (make-tree 11 nil nil))))
(define TR2 (make-tree 3
                       (make-tree 1 nil nil)
                       (make-tree 7
                                  (make-tree 5 nil nil)
                                  (make-tree 9
                                             nil
                                             (make-tree 11 nil nil)))))
(define TR3 (make-tree 5
                       (make-tree 3
                                  (make-tree 1 nil nil)
                                  nil)
                       (make-tree 9
                                  (make-tree 7 nil nil)
                                  (make-tree 11 nil nil))))

(tree->list-1 TR1)
(tree->list-1 TR2)
(tree->list-1 TR3)

(tree->list-2 TR1)
(tree->list-2 TR2)
(tree->list-2 TR3)
