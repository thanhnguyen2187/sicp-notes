; Exercise 2.32
;
; We can represent a set as a list of distinct elements, and we can represent
; the set of all subsets of the set as a list of lists. For example, if the set
; is (1 2 3) then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1
; 2 3)). Complete the following definition of a procedure that generates the set
; of subsets of a set and give a clear explanation of why it works:
; 
; ...

#lang sicp

(define (car-default item)
  (if (pair? item)
      (car item)
      nil))

(define (subsets s)
  ; (display s)
  ; (newline)
  ; - The base case is when `s` is null, we trivially return a blank list within
  ; a list
  ; - The case where `s` has one elements `x1`, we have `() (x1)`
  ; - The case where `s` has two elements `x1` and `x2`, we treat it as two cases:
  ;   + Where `s` only have one element `x2`, we have `() (x2)`
  ;   + We append `x1` to each of the calculated cases, we have `(x1) (x1 x2)`
  ;   + Put the two cases together, the result is `(x1) (x1 x2) () (x2)`
  ; - The case where `s` has three elements `x1`, `x2`, and `x3`, we continues
  ; treating it as two cases:
  ;   + Where `s` has two elements `x2` and `x3`: we use the previous
  ;   calculation to have `(x2) (x2 x3) () (x3)` 
  ;   + Where `s` has three elements `x1`, `x2`, and `x3`, we continue appending
  ;   `x1` to the result where `s` has two elements to have: `(x1 x2) (x1 x2 x3)
  ;   (x1) (x1 x3)`
  ;   + Put the two cases together, we have `...`
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (rest)
                       (append (list (car s))
                               rest))
                     rest)))))
; (subsets (list 1 2 3))
; --> (subsets (list 2 3))
; ------> (subsets (list 3))
; ---------> (subsets (list))
; -> 
; (f a-1 a-2 a-3 . a-n)
; append(descarte-product (f a-1 a-2 a-3 . a-n-1)
;        (f a-n))
