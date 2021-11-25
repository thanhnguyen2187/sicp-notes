; Exercise 4.3
;
; Rewrite `eval` so that the dispatch is done in data-directed style. Compare
; this with the data-directed differentiation procedure of Exercise 2.73. (You
; may use the `car` of a compound expression as the type of the expression, as
; is appropriate for the syntax implemented in this section.)

#lang sicp

(#%require (only racket require))
(require "./exercise-4.0.rkt")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply-local (eval (operator exp) env)
                      (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))
