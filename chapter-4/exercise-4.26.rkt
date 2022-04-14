; Exercise 4.26
;
; Ben Bitdiddle and Alyssa P. Hacker disagree over the importance of lazy
; evaluation for implementing things such as `unless`. Ben points out that it's
; possible to implement `unless` in applicative order as a special form.
;
; Alyssa counters that, if one did that, `unless` would be merely syntax, not a
; procedure that could be used in conjunction with higher-order procedures.
;
; Fill in the details on both sides of the argument.
;
; Show how to implement `unless` as a derived expression (like `cond` or `let`),
; and give an example of a situation where it might be useful to have `unless`
; vailable as a procedure, rather than a special form.
;
; ---
;
; > It's possible to implement `unless` in applicative order as a special form.
;
; > If one did that, `unless` would be merely syntax, not a procedure that could
; be used in conjunction with higher-order procedures.

#lang sicp

(define-syntax unless
  (syntax-rules ()
    ((unless predicate
       first-value
       second-value)
     (if (not predicate)
       first-value
       second-value))))

(if true
  (display "true")
  (display "false"))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

(define (unless-applicative condition
                            usual-value
                            exceptional-value)
  (if condition
    exceptional-value
    usual-value))

(let
  ((select-y (list #t #t #t #t))
   (xs (list 'x1 'x2 'x3 'x4))
   (ys (list 'y1 'y2 'y3 'y4)))
  (map unless-applicative
       select-y
       xs
       ys))

(factorial 5)

