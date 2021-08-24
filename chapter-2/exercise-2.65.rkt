; Exercise 2.65
;
; Use the resluts of Exercise 2.63 and Exercise 2.64 to give Omega(n)
; implementations of `union-set` and `intersection-set` for sets implemented as
; (balanced) binary trees.

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

(define (union-set set1 set2)
  (define (iterate set1 set2 result)
    (cond ((null? set1) (append result set2))
          ((null? set2) (append result set1))
          (else (let ((x1 (car set1))
                      (x2 (car set2)))
                  (cond ((< x1 x2) (iterate (cdr set1)
                                            set2
                                            (cons x1 result)))
                        ((= x1 x2) (iterate (cdr set1)
                                            (cdr set2)
                                            (cons x1 result)))
                        ((> x1 x2) (iterate set1
                                            (cdr set2)
                                            (cons x2 result))))))))
  (list->tree (iterate set1 set2 (list))))

(define (union-set-v2 set1 set2)
  (define (iterate set1 set2 result)
    (cond ((null? set1) (append result
                                (tree->list set2)))
          ((null? set2) (append result
                                (tree->list set1)))
          (else (let ((x1 (car set1))
                      (x2 (car set2)))
                  (cond ((< x1 x2) (iterate (cdr set1)
                                            set2
                                            (cons x1 result)))
                        ((= x1 x2) (iterate (cdr set1)
                                            (cdr set2)
                                            (cons x1 result)))
                        ((> x1 x2) (iterate set1
                                            (cdr set2)
                                            (cons x2 result)))))))))

(define (intersection-set set1 set2)
  (define (iterate set1 set2 result)
    (if (or (null? set1)
            (null? set2))
        result
        (let ((x1 (car set1))
              (x2 (car set2)))
          (cond ((< x1 x2) (iterate (cdr set1)
                                    set2
                                    result))
                ((= x1 x2) (iterate (cdr set1)
                                    (cdr set2)
                                    (cons x1 result)))
                ((> x1 x2) (iterate set1
                                    (cdr set2)
                                    result))))))
  (iterate set1 set2 (list)))

(define (list->tree elements)
  (car (partial-tree elements
                     (length elements))))

(define (partial-tree elts n)
  ; Take as arguments an integer /n/ and list of at least /n/ elements and
  ; construct a balanced tree containing the first /n/ elements of the list.
  ;
  ; The result returned by `partial-tree` is a pair whose `car` is the
  ; constructed tree and whose `cdr` is the list of elements not included in the
  ; tree.
  ;
  ; The steps:
  ; - Split the elements in left half, middle element, and right half
  ; - Build the left tree with the left half
  ; - Build the right tree with the right half
  ; - Build the current tree with the middle element, left half, and right half
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1)
                                 2)))
        (let ((left-result (partial-tree elts
                                         left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n
                               (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(define (tree->list tree)
  (define (copy-to-list tree
                        result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(union-set (list 1 2 3 4) (list 3 4 5 6 7))
(intersection-set (list 1 2 3 4) (list 3 4 5 6 7))
