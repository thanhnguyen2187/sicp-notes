; Using `find-variable` from Exercise 5.41, rewrite `compile-variable` and
; `compile-assignment` to output lexical-address instructions.
;
; In cases where `find-variable` returns `not-found` (that is, where the
; variable is not in the compile-time environment), you should have the code
; generators use the evaluator operations, as before, to search for the binding.
; (The only place a variable is not found at compile time can be in the global
; environment, which is part of the run-time environment but is not part of the
; compile-time environment. Thus, if you wish, you may have the evaluator
; operations look directly in the global environment, which can be obtained with
; the operation `(op get-global-environment)`, instead of having them search the
; whole run-time environment and found in `env`.) Test the modified compiler on
; a few simple cases, such as the nested `lambda` combination at the beginning
; of this section.

(load "ch5-compiler.scm")

(define (make-lexical-address frame-number displacement-number)
  (list frame-number displacement-number))

(define (find-variable variable compile-time-env)
  (define (find-in-frame variable frame)
    (define (iterate index variable frame)
      (cond ((null? frame) 'not-found)
            ((eq? variable (car frame)) index)
            (else (iterate (+ index 1) variable (cdr frame)))))
    (iterate 0 variable frame))
  (define (iterate frame-number
                   variable
                   compile-time-env)
    (if (null? compile-time-env)
      'not-found
      (let* ((frame (car compile-time-env))
             (found-displacement-number (find-in-frame variable frame)))
        (if (eq? found-displacement-number 'not-found)
          (iterate (+ frame-number 1)
                   variable
                   (cdr compile-time-env))
          (make-lexical-address frame-number found-displacement-number)))))
  (iterate 0 variable compile-time-env))

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

(define (compile-variable exp target linkage compile-time-env)
  (let ((address (find-variable exp compile-time-env))
        (seq (make-instruction-sequence
               '(env) (list target unev)
               `((assign ,target
                         ; (op lookup-variable-value)
                         (op lookup-variable-value)
                         (const ,exp)
                         (reg env))))))
    (end-with-linkage linkage seq))
  )

(define (compile-assignment exp target linkage compile-time-env)
  (let ((var (assignment-variable exp))
        (get-value-code (compile (assignment-value exp)
                                 'val
                                 'next
                                 compile-time-env)))
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
