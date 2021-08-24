; Exercise 2.78
;
; The internal procedures in the `scheme-number` package are essentially nothing
; more than calls to the primitive procedures `+`, `-`, `*`, `/`, etc. It was
; not possible to use the primitives of the language directly because of our
; type-tag system requires that each data object have a type attached to it. In
; fact, however, all Lisp implementations do have a type system, which they use
; internally. Primitive predicates such as `symbol?` and `number?` determine
; whether data objects have particular types.
;
; Modify the definitions of `type-tag`, `contents`, and `attach-tag` from
; Section 2.4.2 so that our generic system takes advantages of Scheme's internal
; type system.
;
; That is to say, the system should work as before except that ordinary numbers
; should be represented simply as Scheme numbers rather than as pairs whose
; `car` is the symbol `scheme-number`.

#lang sicp

(define (put) 0)
(define (get) 0)

(define (attach-tag type-tag contents) 
  (if (number? contents)
      contents
      (cons (type-tag contents))))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) datum)
        (error "Bad tagged datum: TYPE-TAG")))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (error "Bad tagged datum: CONTENTS")))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)


