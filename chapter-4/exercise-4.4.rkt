; Exercise 4.4
;
; Recall the definitions of the special forms `and` and `or` from Chapter 1:
;
; - `and`: The expressions are evaluated form left to right. If any expression
; evaluates to false, false is returned; any remaining expressions are not
; evaluated. If all the expressions evaluated to true values, the value of the
; last expression is returned. If there are no expressions then true is
; returned.
; - `or`: The expressions are evaluated from left to right. If any expression
; evaluates to a true value, that value is returned; any remaining expressions
; are not evaluated. If all expressions evaluate to false, or if there are no
; expressions, then false is returned.
;
; Install `and` and `or` as new special forms for the evaluator by defining
; appropriate syntax procedures and evaluation procedures `eval-and` and
; `eval-or`. Alternatively, show how to implement `and` and `or` as derived
; expressions.

#lang sicp

(#%require (only racket require))
(require "./exercise-4.0.rkt")

(define (eval exp env)
  (display exp) (newline)
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
        ((and? exp)
         (eval-and (operands exp) env))
        ((application? exp)
         (apply-local (eval (operator exp) env)
                      (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (and? exp)
  (tagged-list? exp 'and))

(define (eval-and exp env)
  (display exp) (newline)
  (if (null? exp)
    true
    (let ((first-clause (car exp)))
      (if (false? (eval first-clause env))
          false
          (eval-and (cdr exp)
                    env)))))

;; (define the-global-environment (setup-environment))
(driver-loop)
