; Exercise 2.53
;
; What would the interpreter print in response to evaluating each of the
; following expressions?
; 
; ...

#lang sicp

(list 'a 'b 'c)
; (a b c)
(list (list 'george))
; ((geogre))
(cdr '((x1 x2) (y1 y2)))
; (x1 x2)
(cadr '((x1 x2) (y1 y2)))
; (y1 y2)
(pair? (car '(a short list)))
; #f

(memq 'red '((red shoes) (blue socks)))
; #f
(memq 'red '(red shoes blue socks))
; (red shoes blue socks)
