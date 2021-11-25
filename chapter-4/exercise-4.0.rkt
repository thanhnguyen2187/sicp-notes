;;;;METACIRCULAR EVALUATOR FROM CHAPTER 4 (SECTIONS 4.1.1-4.1.4) of
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm

;;;;This file can be loaded into Scheme as a whole.
;;;;Then you can initialize and start the evaluator by evaluating
;;;; the two commented-out lines at the end of the file (setting up the
;;;; global environment and starting the driver loop).

;;;;**WARNING: Don't load this file twice (or you'll lose the primitives
;;;;  interface, due to renamings of apply).

#lang sicp

(#%require (only racket require provide all-defined-out))
(provide (all-defined-out))
;; (require racket/pretty)

;;;from section 4.1.4 -- must precede def of metacircular apply
(define apply-in-underlying-scheme apply)

;;;SECTION 4.1.1

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
        ((and? exp)
         (eval-and (operands exp) env))
        ((or? exp)
         (eval-or (operands exp) env))
        ((let? exp)
         (eval (let->combination exp) env))
        ((let*? exp)
         (eval (let*->nested-lets exp) env))
        ((application? exp)
         (apply-local (eval (operator exp) env)
                      (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply-local procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;;;SECTION 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause)
  ;; Exercise 4.5
  (if (tagged-list? (cdr clause) '=>)
    (cddr clause)
    (cdr  clause))
  )

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
                     (expand-clauses rest))
            ))))

;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;[do later]
;(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;[moved to start of file] (define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))



(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;; Exercise 4.4

(define (and? exp)
  (tagged-list? exp 'and))

(define (eval-and exp env)
  (if (null? exp)
    true
    (let ((first-clause (car exp)))
      (if (false? (eval first-clause env))
          false
          (eval-and (cdr exp)
                    env)))))

(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-or exp env)
  (if (null? exp)
    false
    (let ((first-clause (car exp)))
      (if (true? (eval first-clause env))
          true
          (eval-or (cdr exp)
                   env)))))

;; Exercise 4.6

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-vars exp)
  (map car (cadr exp)))

(define (let-exps exp)
  (map cadr (cadr exp)))

(define (let-body exp)
  (cddr exp))

;; Exercise 4.8

(define (named-let? exp)
  (and (let? exp)
       (variable? (cadr exp))))

(define (named-let-vars exp)
  (map car (caddr exp)))

(define (named-let-exps exp)
  (map cadr (caddr exp)))

(define (named-let-name exp)
  (cadr exp))

(define (named-let-body exp)
  (cadddr exp))

(define (make-define variable value)
  (list 'define variable value))

(define (make-define-procedure name variables body)
  (make-define (cons name variables) body))

;;

(define (let->combination exp)
  ;; Exercise 4.8
  (if (named-let? exp)
      (let ((name (named-let-name exp))
            (vars (named-let-vars exp))
            (exps (named-let-exps exp))
            (body (named-let-body exp)))
        (make-begin
          (list (make-define-procedure name
                                       vars
                                       body)
                (cons name exps))))
      (cons (make-lambda (let-vars exp)
                         (let-body exp))
            (let-exps exp)))
  )

;; Exercise 4.7

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*-vars exp)
  (cadr exp))

(define (let*-body exp)
  (caddr exp))

(define (make-let* vars body)
  (list 'let* vars body))

(define (let*->nested-lets exp)
  (let ((vars (let*-vars exp))
        (body (let*-body exp)))
    (if (null? vars)
        body
        (list 'let (cons (car vars) nil)
              (let*->nested-lets
                (make-let* (cdr vars)
                           body))))))

;; Exercise 4.8

;; Exercise 4.9

(define (while? exp)
  (tagged-list? exp 'while))

(define (while-condition exp)
  (cadr exp))

(define (while-body exp)
  (caddr exp))

(define (while->combination exp)
  (make-if (while-condition exp)
           (while-body exp)
           nil)
  )

;;;Following are commented out so as not to be evaluated when
;;; the file is loaded.
(define the-global-environment (setup-environment))
;; (driver-loop)

;; REPL Tests

(define test-let '(let ((x 0) (y 1)) 10))
(display (let-vars test-let))
(display (let-exps test-let))
(display (let-body test-let))
(display (let->combination test-let))

(define test-let* '(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
                     (* x z)))
(display (let*-vars test-let*))
(display (let*-body test-let*))
(display (let*->nested-lets test-let*))

(define test-named-let '(let f ((a 1) (b 2)) a))
(display (named-let? test-named-let)) ; #t
(display (named-let? test-let)) ; #f
(display (named-let-name test-named-let)) ; f
(display (named-let-vars test-named-let)) ; (a b)
(display (named-let-exps test-named-let)) ; (1 2)
(display (named-let-body test-named-let)) ; a

(display (let->combination test-named-let)) ; ((define (f a b) a) (f 1 2))

(display (make-define-procedure 'f '(a b) 'b))

(define test-for '(for ((x 0) (< x 10) inc) (display x)))

(define test-while '(while (< x 0) (inc x)))
(display test-while)
(display (while? test-while))
(display (while-condition test-while))
(display (while-body test-while))
(display (while->combination test-while))

(display
  (eval
    '(let ((x 0) (y x)) 10)
    the-global-environment))

(display
  (eval
    '(let* ((x 2) (y x)) y)
    the-global-environment))

(display
  (eval
    '(let f ((a 1) (b 2)) a)
    the-global-environment))

