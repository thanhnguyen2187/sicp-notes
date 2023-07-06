; Modify the compiler to maintain the compile-time environment as described
; above. That is, add a compile-time environment argument to `compile` and the
; various code generators, and extend it in `compile-lambda-body`.
;
; ---
;
; > One way for the compiler to produce code that uses lexical addressing is to
; maintain a data structure called *compile-time environment*. This keeps track
; of which variables will be at which positions in which frames in the run-time
; environment when a particular variable-access operation is executed. The
; compile-time environment is a list of frames, each containing a list of
; variables. (There will of course be no values bound to the variables, since
; values are not computed at compile time.)
;
; > The compile-time environment becomes an additional argument to `compile` and
; is passed along to each code generator. The top-level call to `compile` uses
; an empty compile-time environment. When a `lambda` body is compiled,
; `compile-lambda-body` extends the compile-time environment by a frame
; containing the procedure's parameters, so that the sequence making up the body
; is compiled with that extended environment. At each point in the compilation,
; `compile-variable` and `compile-assignment` use the compile-time environment
; in order to generate the appropriate lexical address.

(define (compile exp target linkage compile-time-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage compile-time-env))
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

(define (compile-assignment exp target linkage compile-time-env)
  (let ((var (assignment-variable exp))
        (get-value-code
          (compile (assignment-value exp) 'val 'next compile-time-env)))
    (end-with-linkage
      linkage
      (preserving
        '(env)
        get-value-code
        (make-instruction-sequence '(env val) (list target)
                                   `((perform (op set-variable-value!)
                                              (const ,var)
                                              (reg val)
                                              (reg env))
                                     (assign ,target (const ok))))))))

(define (compile-definition exp target linkage compile-time-env)
  (let ((var (definition-variable exp))
        (get-value-code
          (compile (definition-value exp) 'val 'next compile-time-env)))
    (end-with-linkage
      linkage
      (preserving
        '(env)
        get-value-code
        (make-instruction-sequence '(env val) (list target)
                                   `((perform (op define-variable!)
                                              (const ,var)
                                              (reg val)
                                              (reg env))
                                     (assign ,target (const ok))))))))


(define (compile-if exp target linkage compile-time-env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next compile-time-env))
            (c-code (compile (if-consequent exp) target consequent-linkage compile-time-env))
            (a-code (compile (if-alternative exp) target linkage compile-time-env)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                      (make-instruction-sequence '(val) '()
                                                 `((test (op false?) (reg val))
                                                   (branch (label ,f-branch))))
                      (parallel-instruction-sequences
                        (append-instruction-sequences t-branch c-code)
                        (append-instruction-sequences f-branch a-code))
                      after-if))))))

(define (compile-lambda exp target linkage compile-time-env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage (if (eq? linkage 'next)
                            after-lambda
                            linkage)))
      (append-instruction-sequences
        (tack-on-instruction-sequence
          (end-with-linkage
            lambda-linkage
            (make-instruction-sequence '(env) (list target)
                                       `((assign ,target
                                                 (op make-compiled-procedure)
                                                 (label ,proc-entry)
                                                 (reg env)))))
          (compile-lambda-body exp proc-entry compile-time-env))
        after-lambda))))

(define (compile-lambda-body exp proc-entry compile-time-env)
  (let ((formals (lambda-parameters exp))
        (extended-env (cons formals compile-time-env)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        ; (assign env (op compiled-procedure-env) (reg proc))
        ; (assign env
        ;         (op extend-environment)
        ;         (const ,formals)
        ;         (reg argl)
        ;         (reg env))
        ))
     (compile-sequence (lambda-body exp) 'val 'return extended-env))))

(define (compile-sequence seq target linkage compile-time-env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage compile-time-env)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next compile-time-env)
       (compile-sequence (rest-exps seq) target linkage compile-time-env))))

(define (compile-application exp target linkage compile-time-env)
  (let ((proc-code (compile (operator exp) 'proc 'next compile-time-env))
        (operand-codes (map (lambda (operand) (compile operand 'val 'next compile-time-env))
                            (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

