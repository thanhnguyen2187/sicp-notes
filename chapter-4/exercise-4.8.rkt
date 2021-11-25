; Exercise 4.8
;
; "Named `let`" is a variant of `let` that has the form
;
; ...
;
; The <bindings> and <body> are just as in ordinary `let`, except that <var> is
; bound within <body> to a procedure whose body is <body> and whose parameter
; are the variables in the <bindings>. Thus, one can repeatly execute the <body>
; by invoking the procedure named <var>. For example, the iterative Fibonacci
; procedure (Section 1.2.2) can be rewritten using named `let` as follows:
;
; ...
;
; Modify `let->combination` of Exercise 4.6 to also support named `let`.

#lang sicp
