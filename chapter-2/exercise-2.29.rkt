; Exercise 2.29
;
; A binary mobile consists of two branches, a left branch and a right branch.
; Each branch is a rod of a certain length, from which hangs either a weight or
; another binary mobile.
;
; We can represent a binary mobile using compound data by constructing it from
; two branches (for example, using `list`):
;
; ...
;
; A branch is constructed from a `length` (which must be a number) together with
; a `structure`, which may be either a number (representing a simple weight) or
; another mobile:
;
; ...
;
; a. Write corresponding selectors `left-branch` and `right-branch`, which
; return the branches of a mobile, and `branch-length` and `branch-structure`,
; which return the components of a branch.
;
; b. Using your selectors, define a procedure `total-weight` that returns the
; total weight of a mobile.
;
; c. A mobile is said to be /balanced/ if:
;
; - the torque applied by its top-left branch is equal to that applied by its
; top-right branch (that is, if the length of the left rod multiplied by the
; weight hanging from that rod is equal to the corresponding product for the
; right side)
; - and if each of the submobiles hanging off its branches is balanced.
;
; Design a predicate that tests whether a binary mobile is balanced.
;
; d. Suppose we change the representation of mobiles so that the constructors are
;
; ...
;
; How much do you need to change your programs to convert the new
; representation?

#lang sicp

(define (make-mobile left right)
  (list left right))
(define (make-branch length
                     structure)
  (list length structure))

; a.

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (branch? item)
  (and (pair? item)
       (not (pair? (car item)))))

(define (mobile? item)
  (and (pair? item)
       (pair? (car item))
       (pair? (cadr item))))

(define (total-weight item)
  ; (display item)
  ; (newline)
  (cond ((branch? item)
         (if (not (mobile? (branch-structure item)))
             (branch-structure item)
             (total-weight (branch-structure item))))
        ((mobile? item)
         (+ (total-weight (left-branch item))
            (total-weight (right-branch item))))
        (else 0)))

(define (torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (let ((top-left (left-branch mobile))
        (top-right (right-branch mobile)))
    (and (= (torque top-left)
            (torque top-right)))))

(define B (make-branch 6
                       (make-mobile (make-mobile (make-branch 8 8)
                                                 (make-branch 8 8))
                                    (make-branch 3 3))))
(define M (make-mobile (make-branch 3 3)
                       (make-branch 3 3)))
(total-weight B)
(balanced? M)
