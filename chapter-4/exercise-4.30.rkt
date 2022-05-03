; Exercise 4.30
;
; Cy D. Fect, a reformed C programmer, is worried that some side effects may
; never take place, because the lazy evaluator doesn't force the expressions in
; a sequence.
;
; Since the value of an expression in a sequence other than the last one is not
; used (the expression is there only for its effect, such as assigning to a
; variable or printing), there can be no subsequent use of this value (e.g., as
; an argument to a primitive procedure) that will cause it to be forced.
;
; Cy thus thinks that when evaluating sequences, we must force all expressions
; in the sequence except the final one. He proposes to modify `eval-sequence`
; from Section 4.1.1. to use `actual-value` rather than `eval`:
;
; ...
;
; a. Ben Bitdiddle thinks Cy is wrong. He shows Cy the `for-each` procedure
; described in Exercise 2.23, which gives an important example of a sequence
; with side effects:
;
; ...
;
; He claims that the evaluator in the text (with the original `eval-sequence`)
; handles this correctly:
;
; ...
;
; Explain why Ben is right about the behavior of `for-each`
;
; b. Cy agrees that Ben is right about the `for-each` example, but says that
; that's not the kind of program he was thinking about when he proposed his
; change to `eval-sequence`. He defines the following two procedures in the lazy
; evaluator:
;
; ...
;
; What are the values of `(p1 1)` and `(p2 1)` with the original
; `eval-sequence`? What would the values be with Cy's proposed change to
; `eval-sequence`?
;
; c. Cy also points out that changing `eval-sequence` as he proposes does not
; affect the behavior of the example in part a. Explain why this is true.
;
; d. How do you think sequences ought to be treated in the lazy evaluator? Do
; you like Cy's approach, the approach in the text, or some other approach.
;
; ---
;
; a. Ben is right about the behavior of `for-each` since all of the `proc` he
; used has side effects in them and are evaluated sequentially.
;
; b. Using the original `eval-sequence`, the returned values of `(p1 1)` and `(p2
; 1)` are `(1 2)`, and `1`. Using Cy's propsed change, the returned values are
; `(1 2)` and `(1 2)`.
;
; c. It is true since Cy's change does not make a difference to the fact that
; Ben's code contains side effects.
;
; d. The two approaches have their own pros and cons. A better approach would be
; to let the user freely specify that which expression needs to be
; lazily-evaluated, or memoized, or used normally.

#lang sicp

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
          (actual-value (first-exp exps) env))))

(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '2))))
