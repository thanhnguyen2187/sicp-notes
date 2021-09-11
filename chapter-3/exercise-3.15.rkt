; Exercise 3.15
;
; Draw box-and-pointer diagrams to explain the effect of `set-to-wow!` on the
; structures of `z1` and `z2` above.
;
; ---
;
; z1: [. .]
;      | |
;      |-+
;      |
;      v
;  x: [. .] -> [. /]
;      |        |
;      v        v
;      a        b
;

#lang sicp

(define x (list 'a 'b))
(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x) (set-car! (car x) 'wow) x)

(set-to-wow! z1)
(set-to-wow! z2)
(cons 'a 'b)
