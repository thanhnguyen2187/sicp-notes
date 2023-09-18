; Use the compiler to compile the metacircular evaluator of Section 4.1 and run
; this program using the register-machine simulator. (To compile more than one
; definition at a time, you can package the definitions in a `begin`.) The
; resulting interpreter will run very slowly because of the multiple levels of
; interpretation, but getting all the details to work is an instructive
; exercise.

(load "ch5-compiler")
(load "load-eceval-compiler")
(load "format-compiled-code")

;; Modify the compiler

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
;;      more primitives
        (list 'not not)
        (list 'apply apply)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'display display)
        (list 'newline newline)
        (list 'read read)
        (list 'pair? pair?)
        (list 'eq? eq?)

        (list 'symbol? symbol?)
        (list 'string? string?)
        (list 'number? number?)
        (list 'list list)

        (list 'cadr cadr)
        (list 'cddr cddr)
        (list 'caddr caddr)
        (list 'cdddr cdddr)
        (list 'caadr caadr)
        (list 'cdadr cdadr)
        (list 'cadddr caddr)
        (list 'cddddr cdddr)
        (list 'length length)
        (list 'set-car! set-car!)
        (list 'set-cdr! set-cdr!)
        ))

;; from chapter 4 to turn `let` expression to `lambda`

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-var binding) (car binding))
(define (let-val binding) (cadr binding))

(define (make-combination operator operands) (cons operator operands))

(define (let->combination exp)
  ;;make-combination defined in earlier exercise
  (let ((bindings (let-bindings exp)))
    (make-combination (make-lambda (map let-var bindings)
                                   (let-body exp))
                      (map let-val bindings))))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ;; Exercise 5.50
        ((let? exp)
         (compile-application (let->combination exp) target linkage))
        ;;
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

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

;;;; Metacircular Evaluator

(define apply-in-underlying-scheme (cadr apply))

; load from `ch4-mceval`, minus the definition of `apply-in-underlying-scheme`

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
;;      more primitives
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        ))

(define the-global-environment (setup-environment))
; (define the-global-environment '())

(driver-loop)

(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(factorial 5)

