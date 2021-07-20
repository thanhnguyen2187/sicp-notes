; Exercise 1.34
;
; Suppose we define the procedure
;
; ...
;
; Then we have
; 
; ...
;
; What happens if we (perversely) ask the interpreter to evaluate the
; combination `(f f)`? Explain.

#lang sicp

(define (f g) (g 2))

(define (square x) (* x x))

(f square)

(f (lambda (z)
     (* z
        (+ z 1))))

(f f)
