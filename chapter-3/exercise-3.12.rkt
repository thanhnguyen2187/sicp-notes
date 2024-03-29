; Exercise 3.12
;
; The following procedure for appending lists was introduced in Section 2.2.1:
;
; ...
;
; `append` forms a new list by successively `cons`ing the elements of x onto y.
; The procedure `append!` is similar to `append`, but it is a mutator rather
; than a constructor. It appends the lists by splicing them together, modifying
; the final pair of x so that its `cdr` is now `y`. (It is an error to call
; `append!` with an empty `x`.)
;
; ...
;
; Here `last-pair` is a procedure that returns the last pair in its argument:
;
; ...
;
; Consider the interaction
;
; ...
;
; What are the missing `responses`? Draw box-and-pointer diagrams to explain
; your answer.

#lang sicp

(define (last-pair) 0)

(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

z
; (a b c d)

(cdr x)
; <response>
; (b)
; x: [a] -> [b]
; y: [c] -> [d]
;
; After the call, `x` is not modified.

(define w (append! x y))
; (a b c d)

(cdr x)
; <response>
; (b c d)
;
; After the call, `x` is modified.
