; Exercise 3.14
;
; The following procedure is quite useful, although obscure:
;
; ...
;
; `loop` uses the "temporary" variable `temp` to hold the old value of the `cdr`
; of `x`, since the `set-cdr!` on the next line destroys the `cdr`.
;
; Explain what `mystery` does in general. Suppose `v` is defined by
;
; ...
;
; Draw the box-and-pointer diagram that represents the list to which `v` is
; bound. Suppose that we now evaluate `(define w (mystery v))`. Draw
; box-and-pointer diagrams that show the structures `v` and `w` after evaluating
; this expression.
;
; What would be printed as the values of `v` and `w`?

#lang sicp

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
; (mystery (list 'a 'b 'c 'd))
; (loop (list 'a 'b 'c 'd) '())
; (loop (list 'b 'c 'd) (list 'a))
; (loop (list 'c 'd) (list 'b 'a))
; (loop (list 'd) (list 'c 'b 'a))
; (loop (list) (list 'd 'c 'b 'a))
; (list 'd 'c 'b 'a)

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

v
; (a)
w
; (d c b a)
