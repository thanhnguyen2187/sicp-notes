; Modify the evaluator so that it uses normal-order evaluation, based on the
; lazy evaluator of Section 4.2.

(load "load-eceval.scm")
; or import from ch5-19 for register tracing

(define the-global-environment (setup-environment))

(define eceval-operations
  (list
   ;;primitive Scheme operations
   (list 'read read)

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
   ;; Exercise 5.23
   (list 'cond? cond?)
   ;; Exercise 5.24
   (list 'cond-no-clause? cond-no-clause?)
   (list 'cond-else-pred? cond-else-pred?)
   (list 'cond-first-pred cond-first-pred)
   (list 'cond-first-actions cond-first-actions)
   (list 'tag-begin tag-begin)
   (list 'reduce-cond reduce-cond)
   ;; Exercise 5.25
   (list 'thunk? thunk?)
   (list 'thunk-exp thunk-exp)
   (list 'thunk-env thunk-env)
   (list 'evaluated-thunk? evaluated-thunk?)
   (list 'thunk-value thunk-value)
   (list 'set-car! set-car!)
   (list 'set-cdr! set-cdr!)
   (list 'car car)
   (list 'cdr cdr)
   (list 'cons cons)
   (list 'list list)
   ; (list 'force-it force-it)
   ; (list 'delay-it delay-it)
   ; (list 'actual-value actual-value)
   ; (list 'list-of-arg-values list-of-arg-values)
   ; (list 'list-of-delayed-args list-of-delayed-args)
   ;;

   ;;operations in eceval-support.scm
   (list 'true? true?)
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
   (list 'get-global-environment get-global-environment))
   )

;; Exercise 5.24

(define (cond-no-clause? exp)
  (null? (cond-clauses exp)))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-first-pred exp)
  (caar (cond-clauses exp)))

(define (cond-first-actions exp)
  (cdar (cond-clauses exp)))

(define (reduce-cond exp)
  (cons 'cond (cdr (cond-clauses exp))))

(define (cond-else-pred? exp)
  (eq? exp 'else))

(define (tag-begin exp)
  (cons 'begin exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;; Exercise 5.25

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))
;; Testing

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
  '(
;;SECTION 5.4.4
read-eval-print-loop
  (perform (op initialize-stack))
  (perform
   (op prompt-for-input) (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label eval-dispatch))
print-result
;;**following instruction optional -- if use it, need monitored stack
  (perform (op print-stack-statistics))
  (perform
   (op announce-output) (const ";;; EC-Eval value:"))
  ;; Exercise 5.25
  ; (perform (op force-it) (reg val))
  (assign exp (reg val))
  (assign continue (label print-result-1))
  (goto (label force-it))
print-result-1
  ;;
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

unknown-expression-type
  (assign val (const unknown-expression-type-error))
  (goto (label signal-error))

unknown-procedure-type
  (restore continue)
  (assign val (const unknown-procedure-type-error))
  (goto (label signal-error))

signal-error
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

;; Exercise 5.25

force-it
  ; 1. check if `exp` is a thunk
  (test (op thunk?) (reg exp))
  (branch (label force-it-thunk))
  ; 2. check if `exp` is an evaluated thunk
  (test (op evaluated-thunk?) (reg exp))
  (branch (label force-it-evaluated-thunk))
  ; 3. just return `exp`
  (assign val (reg exp))
  (goto (reg continue))
force-it-thunk
  ; assume that `env` is always the right one before `force-it` is called
  ; (save env)
  ; (assign env (op thunk-env) (reg exp))
  (save exp)
  (assign exp (op thunk-exp) (reg exp))
  (save continue)
  (assign continue (label force-it-thunk-1))
  (goto (label actual-value))
force-it-thunk-1
  (restore continue)
  (restore exp)
  ; (restore env)
  (perform (op set-car!) (reg exp) (const evaluated-thunk))
  (assign exp (op cdr) (reg exp))
  (perform (op set-car!) (reg exp) (reg val))
  ; (perform (op set-cdr!) (reg exp) (const '()))
  ; (restore exp)
  ; (assign val (reg exp))
  (goto (reg continue))
force-it-evaluated-thunk
  (assign val (op thunk-value) (reg exp))
  (goto (reg continue))

actual-value
  ; 1. evaluate `exp`
  (save continue)
  (assign continue (label actual-value-1))
  (goto (label eval-dispatch))
actual-value-1
  ; 2. assign `val` to `exp` for forcing
  (restore continue)
  (assign exp (reg val))
  (goto (label force-it))

delay-it
  ; (assign val (op list) (const thunk) (reg exp) (reg env))
  (assign val (op list) (const thunk) (reg exp))
  (goto (reg continue))

;;

;;SECTION 5.4.1
eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  ;; Exercise 5.23
  (test (op cond?) (reg exp))
  (branch (label ev-cond))
  ;;
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))
ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))
ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure)
              (reg unev) (reg exp) (reg env))
  (goto (reg continue))

ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))
ev-appl-did-operator
  (restore unev)
  (restore env)
  ;; Exercise 5.25
  ; force proc
  (assign exp (reg val))
  (assign continue (label ev-appl-did-operator-1))
  (goto (label force-it))
ev-appl-did-operator-1
  ;;
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)
ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  ;; Exercise 5.25
  ; change from evaluating to delaying
  ; (goto (label eval-dispatch))
  (goto (label delay-it))
  ;;
ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))
ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  ;; Exercise 5.25
  ; change from evaluating to delaying
  ; (goto (label eval-dispatch))
  (goto (label delay-it))
  ;;
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))
apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))  
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))

;; Exercise 5.25

; input: argl
; output: unev
; aux: exp, val
list-of-arg-values
list-of-arg-values-loop
  (test (op no-operands?) (reg argl))
  (branch (label list-of-arg-values-base-case))

  (save continue)

  (assign exp (op first-operand) (reg argl))
  (assign argl (op cdr) (reg argl))
  (assign continue (label list-of-arg-values-loop-1))

  (save argl)
  (goto (label force-it))
list-of-arg-values-loop-1
  (restore argl)
  ; (restore env)
  ; (restore continue)

  ; (save continue)
  (save val)
  (assign continue (label list-of-arg-values-after-loop))

  (goto (label list-of-arg-values-loop))
list-of-arg-values-after-loop
  (restore val)
  (restore continue)
  (assign unev (op cons) (reg val) (reg unev))
  (goto (reg continue))
list-of-arg-values-base-case
  (assign unev (const ()))
  (goto (reg continue))

;;

primitive-apply
  ;; Exercse 5.25
  (save continue)
  (assign continue (label primitive-apply-1))
  (goto (label list-of-arg-values))
primitive-apply-1
  (assign argl (reg unev))
  (restore continue)
  ;;
  (assign val (op apply-primitive-procedure)
              (reg proc)
              (reg argl))
  (restore continue)
  (goto (reg continue))

compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment)
              (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))

;;;SECTION 5.4.2
ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))

ev-sequence
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence-last-exp
  (restore continue)
  (goto (label eval-dispatch))

;;;SECTION 5.4.3

ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  ;; Exercse 5.25
  ; (assign exp (op force-it) (reg exp))
  (save continue)
  (assign continue (label ev-if-1))
  (goto (label force-it))
ev-if-1
  (restore continue)
  (assign exp (reg val))
  ;;
  (goto (label eval-dispatch))
ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))
ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))

;; Exercise 5.24
ev-cond
  (save continue)
  (test (op cond-no-clause?) (reg exp))
  (branch (label ev-cond-empty))

  (save exp) ; save `exp` for later usage
  (assign exp (op cond-first-pred) (reg exp))

  ; case 1: (cond (else actions) ...)
  (test (op cond-else-pred?) (reg exp))
  (branch (label ev-cond-actions))

  ; case 2: (cond (predicate actions))
  ; evaluate predicate
  (assign continue (label ev-cond-1))
  (goto (label eval-dispatch))

ev-cond-1
  ; case 2.1, the predicate is truthy
  ; we jump to `ev-cond-actions`, where the actions to reg,
  ; and let `eval-dispatch` do its job
  (test (op true?) (reg val))
  (branch (label ev-cond-actions))

  ; case 2.2, the predicate is falsy
  ; we "reduce" the original `cond` expression,
  ; and return the control to `eval-dispatch` again
  (restore exp)
  (restore continue)
  (assign exp (op reduce-cond) (reg exp))
  (goto (label eval-dispatch))

ev-cond-actions
  (restore exp)
  (assign exp (op cond-first-actions) (reg exp))
  (assign exp (op tag-begin) (reg exp))

  (restore continue)
  (goto (label eval-dispatch))

ev-cond-empty
  ; a dummy value is needed to avoid infinite loop
  (assign exp (const 'no-value))
  (restore continue)
  (goto (label eval-dispatch))

;;

ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch))
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op set-variable-value!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))

ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch))
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))
   )))

((eceval 'reg-tracing-on!) 'proc)
((eceval 'reg-tracing-on!) 'argl)
((eceval 'reg-tracing-on!) 'unev)
((eceval 'reg-tracing-on!) 'val)
((eceval 'reg-tracing-on!) 'exp)
; (set-breakpoint eceval 'eval-dispatch 1)
(eceval 'trace-on!)

(eceval 'trace-off!)

(start eceval)

(+ 1 2 3 4)

(+ 1 (+ 2 3))

(+ 1 2 (+ 3 4 (+ 5 6)))

(define (f x)
  (+ x 1))

(f 1)

(f (+ 1 (+ 3 4)))

(define (try a b)
  (if (= a 0)
    1
    b))

(try 0 (/ 1 0))

(define (lazy-car a b) a)

(lazy-car 1 (/ 1 0))

