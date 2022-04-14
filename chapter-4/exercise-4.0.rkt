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

#| (#%require (only racket require provide all-defined-out)) |#
#| (provide (all-defined-out)) |#
;; (require racket/pretty)

;;;from section 4.1.4 -- must precede def of metacircular apply
(define apply-in-underlying-scheme apply)

(define-syntax ->>
    (syntax-rules ()
        ; Each syntax rule is a pair: a pattern to match on, and an expression to
        ; rewrite to. This first list is the pattern: both args (`->>`, `value`)
        ; are just variables, which we could name however we want. Since the first
        ; part of the pattern matches the macro name, we bind it to `->>`, but `_`
        ; is another popular choice.
        ; So, this pattern matches, e.g., (->> 3)...
        ((->> value)
            ; ...and rewrites to the expression 3, our expected result
            value)

        ; Here we match a more complex pattern, where the second var is a list--
        ; that is, a function call. Note the ellipses, which gather multiple items
        ; into `args` and `rest`.
        ; This would match, e.g., (->> 3 (+ 1 2) (/ 3))...
        ((->> value (fn args ...) rest ...)
            ; ...and rewrite it to (->> (+ 1 2 3) (/ 3)), or (->> 6 (/ 3)).
            ; Note that we just accomplished the "thread-last" part!
            ; Obviously we're not at a final value yet: (->> 6 (/ 3)) will invoke
            ; the macro again. So, we "cycle through," evaluating incrementally,
            ; until we reach a final value.
            (->> (fn args ... value) rest ...))

        ; Finally, here we match a named function in the second position:
        ; e.g.. (->> 3 inc (/ 3))
        ((->> value fn rest ...)
            ; ...and pipe it through just like we did above: (->> 4 (/ 3))
            (->> (fn value) rest ...))))


(define-syntax ->
    (syntax-rules ()
        ((-> value)
            value)

        ((-> value
             (fn args ...)
             rest ...)
         (-> (fn value args ...)
             rest ...))

        ((-> value fn rest ...)
            (-> (fn value) rest ...))))

(define (inc x) (+ x 1))

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
        ((while? exp)
         (eval (while->combination exp) env))
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
  ; Exercise 4.16 c
  ;
  ; `make-procedure` is the better place to put `scan-out-defines` since
  ; `procedure-body` is called by `apply-local`, which means the code gets
  ; evaluated many times over `apply`ing
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p)
  ; (-> p
  ;     (caddr)
  ;     ())
  (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values)
  ;; Exercise 4.11
  ;; (map (lambda (variable value) (cons variable value))
  ;;      variables
  ;;      values)
  )

(define (make-frame-4-11 pairs)
  (cons ))

(define (frame-variables frame)
  (car frame)
  ;; Exercise 4.11
  ;; (map (lambda (pair) (car pair))
  ;;      frame)
  )
(define (frame-values frame)
  (cdr frame)
  ;; Exercise 4.11
  ;; (map (lambda (pair) (cdr pair))
  ;;      frame)
  )

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

  ;; Exercise 4.16 a)
  (if (eq? var '*unassigned*)
    (error "Unassigned LOOKUP-VARIABLE-VALUE " var)
    (env-loop env)))

(define (define-var clause)
  (cadr clause))

(define-var '(define a 1))
(define-body '(define a 1))

(define (define-body clause)
  (caddr clause))

; 528

(define (scan-out-defines body)

  (let* ((defines (filter (lambda (clause)
                            (tagged-list? clause 'define))
                          body))
         (define-vars-undefined (->> defines
                                     (map define-var)
                                     (map (lambda (variable)
                                            (list variable '*undefined*)))))
         (define-bodies-set (map (lambda (clause)
                                   (list 'set!
                                         (define-var clause)
                                         (define-body clause)))
                                 defines))
         (not-defines (filter (lambda (clause)
                                (->> 'define
                                     (tagged-list? clause)
                                     not))
                              body)))
      (-> '(let)
          (append (list define-vars-undefined))
          (append define-bodies-set)
          (append not-defines))))

(display (scan-out-defines '((define y 1) (define x 0) (+ x y))))

(define (filter predicate
                sequence)
  (if (null? sequence)
    nil
    (let ((element       (car sequence))
          (rest-sequence (cdr sequence)))
      (if (predicate element)
        (cons element
              (filter predicate
                      rest-sequence))
        (filter predicate
                rest-sequence)))))

(display (filter (lambda (x) (> x 0))
                 (list -3 -2 -1 0 1 2 3)))

(display (append (list 1 2 3) (list 4)))

(filter (list 1 2 3) (lambda (x) (> x 0)))

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
        ;; Exercise 4.14
        ;; The adding does not work since Scheme's `map` expect a procedure,
        ;; not `(list 'procedure ...)` like the ones we are defining within our
        ;; own evaluator
        ;; (list 'map map)
        ;; (list 'identity identity)
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
  (make-begin (list (make-define-procedure 'loop
                                           '()
                                           (list 'if
                                                 (while-condition exp)
                                                 (make-begin (list (while-body exp)
                                                                   '(loop)))))
                    '(loop))))

(define (for? exp)
  (tagged-list? exp 'for))

(define (for-inits exp)
  (cadr exp))

(define (for-condition exp)
  (caddr exp))

;; Exercise 4.13

(define (make-unbound! var frame)
  (define (scan vars vals)
    (cond ((null? vars)
           (error "No variable found"))
          ((eq? var (car vars))
           (begin
             (set-car!)))
          (else (scan (cdr vars)
                      (cdr vals)))))
  (if (= nil frame)
    (error (string "No variable found "
                   var))
    (scan (frame-variables frame)
          (frame-values frame))))

;; Exercise 4.20

(define (letrec? exp)
  (tagged-list? exp 'letrec))

(define (letrec-bindings exp)
  (cadr exp))

(define (letrec-body exp)
  (caddr exp))

(define (to-internal symbol)
  (string->symbol
    (string-append "__"
                   (symbol->string symbol))))

(define (zip sequence-1
             sequence-2)
  (if (or (null? sequence-1)
          (null? sequence-2))
      '()
      (cons (list (car sequence-1)
                  (car sequence-2))
            (zip  (cdr sequence-1)
                  (cdr sequence-2)))))

(define (letrec->let exp)
  (let* ((bindings        (letrec-bindings exp))
         (body            (letrec-body exp))
         (vars            (map car bindings))
         (internal-vars   (map to-internal vars))
         (init-exps       (map cadr bindings))
         (outer-let-inits (map (lambda (var)
                                 (list var ''*unassigned*)) vars))
         (inner-let-inits (zip internal-vars init-exps))
         (set-exps        (map (lambda (pair)
                                 (list 'set! (car pair) (cadr pair))) (zip vars internal-vars))))
    (list 'let
          outer-let-inits
          (append (list 'let
                        inner-let-inits)
                  set-exps)
          body)))

(display (append (list 1 2 3)
                 (list 4 5 6)))

;; Section 4.1.7

(define (eval-analyze exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted?          exp) (analyze-quoted          exp))
        ((variable?        exp) (analyze-variable        exp))
        ((assignment?      exp) (analyze-assignment      exp))
        ((definition?      exp) (analyze-definition      exp))
        ((if?              exp) (analyze-if              exp))
        ((lambda?          exp) (analyze-lambda          exp))
        ((begin?           exp) (analyze-sequence        (begin-actions exp)))
        ((cond?            exp) (analyze                 (cond->if      exp)))
        ((application?     exp) (analyze-application     exp))
        ((let?             exp) (analyze-let             exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-sequence-4.23 exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs))
           ((car procs) env))
          (else
            ((car procs) env)
            (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence: ANALYZE")
      (lambda (env)
        (execute-sequence procs env)))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;; Exercise 4.22

(define (analyze-let exp)
  (analyze (let->combination exp)))

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

(define test-while '(while (< x 0) (set! x (inc x))))
(display test-while)
(display (while? test-while))
(display (while-condition test-while))
(display (while-body test-while))
(display (while->combination test-while))

(define test-for '(for ((x 0) (> x 10) inc) (display x)))
(display (for-inits test-for))

(define test-letrec '(letrec
                       ((fact (lambda (n)
                                (if (= n 1)
                                  1
                                  (* n
                                     (fact (- n 1))))))) (fact 10)))
(define test-letrec-2 '(letrec
                         ((even? (lambda (n)
                                   (if (= n 0)
                                     true
                                     (odd? (- n 1)))))
                          (odd? (lambda (n)
                                  (if (= n 0)
                                    false
                                    (even? (- n 1))))))
                         (even? 10)))
(display test-letrec)
(display (letrec? test-letrec)) ; #t
(display (letrec-bindings test-letrec)) ; (fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1))))))
(display (letrec-body test-letrec)) ; (fact 10)
(display (letrec->let test-letrec))
(display (letrec->let test-letrec-2))

(display
  (eval
    test-let
    the-global-environment))

(display
  (eval
    '(let* ((x 2) (y x)) y)
    the-global-environment))

(display
  (eval
    '(let f ((a 1) (b 2)) a)
    the-global-environment))

(display
  (eval
    test-while
    the-global-environment))

(display
  (eval
    test-for
    the-global-environment))

(display
  (eval
    '(identity '(1 2 3 4))
    the-global-environment))

(display
  (eval
    '(map identity '(1 2 3 4))
    the-global-environment))

(display the-global-environment)
(display (map (lambda (x y) (+ x y)) '(1 2 3 4) '(2 3 4 5)))

