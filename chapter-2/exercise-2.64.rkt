; Exercise 2.64
;
; The following procedure `list->tree` converts an ordered list into a balanced
; binary tree.
;
; The helper procedure `partial-tree` takes as arguments an interger /n/ and
; list of at least /n/ elements and constructs a balanced tree containing the
; first /n/ elements of the list. The resuult returned by `partial-tree` is a
; pair (formed with `cons`) whose car is the constructed tree and whose `cdr` is
; the list of elements not included in the tree.

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
                (display n)             (newline)
                (display elts)          (newline)
                (display left-size)     (newline)
                (display left-result)   (newline)
                (display left-tree)     (newline)
                (display right-size)    (newline)
                (display right-result)  (newline)
                (display right-tree)    (newline)
                (display non-left-elts) (newline)
                (newline)
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(list->tree (list 1 3 5 7 9 11))
; (quotient 8 2)
; (quotient 9 2)
; (quotient 10 2)
