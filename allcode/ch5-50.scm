; Use the compiler to compile the metacircular evaluator of Section 4.1 and run
; this program using the register-machine simulator. (To compile more than one
; definition at a time, you can package the definitions in a `begin`.) The
; resulting interpreter will run very slowly because of the multiple levels of
; interpretation, but getting all the details to work is an instructive
; exercise.

(load "ch5-compiler")
(load "load-eceval-compiler")
(load "format-compiled-code")

;; Reuse from exercise 5.49

(define (assemble-eceval controller-text)
  (assemble controller-text eceval))

(define eceval-operations
  (list
   ;;primitive Scheme operations
   (list 'read read)			;used by eceval

   ;;used by compiled code
   (list 'list list)
   (list 'cons cons)

   ;;operations in syntax.scm
   (list 'self-evaluating? self-evaluating?)
   (list 'quoted? quoted?)
   (list 'text-of-quotation text-of-quotation)
   (list 'variable? variable?)
   (list 'assignment? assignment?)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'definition? definition?)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   (list 'if? if?)
   (list 'if-predicate if-predicate)
   (list 'if-consequent if-consequent)
   (list 'if-alternative if-alternative)
   (list 'begin? begin?)
   (list 'begin-actions begin-actions)
   (list 'last-exp? last-exp?)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'application? application?)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)

   ;;operations in eceval-support.scm
   (list 'true? true?)
   (list 'false? false?)		;for compiled code
   (list 'make-procedure make-procedure)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'extend-environment extend-environment)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'prompt-for-input prompt-for-input)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)
   (list 'last-operand? last-operand?)
   (list 'no-more-exps? no-more-exps?)	;for non-tail-recursive machine
   (list 'get-global-environment get-global-environment)

   ;;for compiled code (also in eceval-support.scm)
   (list 'make-compiled-procedure make-compiled-procedure)
   (list 'compiled-procedure? compiled-procedure?)
   (list 'compiled-procedure-entry compiled-procedure-entry)
   (list 'compiled-procedure-env compiled-procedure-env)

   ;; Exercise 5.49
   (list 'compile compile)
   (list 'statements statements)
   (list 'assemble assemble-eceval)
   ;;
   ))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev
	 compapp			;*for compiled to call interpreted
	 )
   eceval-operations
  '(
read-eval-print-loop
  (perform (op initialize-stack))
  (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign val (reg exp))
  (assign val (op compile) (reg exp) (const val) (const return))
  (assign val (op statements) (reg val))
  (assign val (op assemble) (reg val))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (reg val))
print-result
;;**following instruction optional -- if use it, need monitored stack
  (perform (op print-stack-statistics))
  (perform (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))
   )))

;;

(start-eceval)

;; metacircular evaluator

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output input)
          ; (output (eval input the-global-environment))
          )
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

; TODO: add `newline` and `display` to the primitive list
