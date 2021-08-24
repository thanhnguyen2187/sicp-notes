; Exercise 2.66
;
; Implement the lookup procedurce for the case where the set of records is
; structured as a binary tree, ordered by the numerical values of the keys.

#lang sicp

(define (entry        tree) (car   tree))
(define (left-branch  tree) (cadr  tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (key record) (car record))
(define (value record) (cadr record))

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let ((middle-record (car   set-of-records))
            (left-records  (cadr  set-of-records))
            (right-records (caddr set-of-records)))
        (cond ((< given-key (key middle-record) (lookup given-key
                                                        left-records)))
              ((= given-key (key middle-record) middle-record))
              ((> given-key (key middle-record) (lookup given-key
                                                        right-records)))))))
