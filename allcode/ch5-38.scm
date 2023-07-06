; Our compiler is clever about avoiding unnecessary stack operations, but it is
; not clever at all when it comes to compiling calls to the primitive procedures
; of the language in terms of the primitive operations supplied by the machine.
;
; For example, consider how much code is compiled to compute `(+ a 1)`: The code
; sets up an argument list in `argl`, puts the primitive addition procedure
; (which is finds by looking up the symbol `+` in the environment) into `proc`,
; and test whether the procedure is primitive or compound.
;
; The compiler always generates code to performs the test, as well as code for
; primitive and compound branches (only one of which will be executed). We have
; not show the part of the controller that implements primitives, but we presume
; that these instructions make use of primitive arithmetic operations in the
; machine's data paths.
;
; Consider how much less code would be generated if the compiler could
; *open-code* primitives--that is, if it could generate code to directly use
; these primitive machine operations. The expression `(+ a 1)` might be compiled
; into something as simple as:
;
; ...
;
; In this exercise we will extend our compiler to support open coding of
; selected primitives. Special-purpose code will be generated for calls to these
; primitive procedures instead of the generate procedure-application code.
;
; In order to support this, we will augment our machine with special argument
; registers `arg1` and `arg2`. The primitive arithmetic operations of the
; machine will take their inputs from `arg1` and `arg2`. The results may be put
; into `val`, `arg1`, or `arg2`.
;
; The compiler must be able to recognie the application of an open-coded
; primitive in the source program. We will augment the dispatch in the `compile`
; procedure to recognize the names of these primitives in addition to the
; reserved words (the special forms) it currently recognizes. For each special
; form our compiler has a code generator. In this exercise we will construct a
; family of code generators for the open-coded primitives.
;
; a. The open-coded primitives, unlike the special forms, all need their
; operands evaluated. Write a code generator `spread-arguments` for use by all
; the open-coding code generators. `spread-arguments` should take an operand
; list and compile the given operands targeted to successive argument registers.
; Note that an operand may contain a call to an open-coded primitive, so
; argument registers will have to be preserved during operand evaluation.
;
; b. For each of the primitive procedures `=`, `*`, `-`, and `+`, write a code
; generator that takes a combination with that operator, together with a target
; and a linkage descriptor, and produces code to spread the arguments into the
; registers and then perform the operation targeted to the given target with the
; given linkage. You need only handle expressions with two operands. Make
; `compile` dispatch to these code generators.
;
; c. Try your new compiler with the `factorial` example. Compare the resulting
; code with the resulting produced without open coding.
;
; d. Extend your code generators for `+` and `*` so that they can handle
; expressions with arbitrary numbers of operands. An expression with more than
; two operands will have to be compiled into a sequence of operations, each with
; only two inputs.
;
; ---

(load "ch5-compiler.scm")
(load "format-compiled-code.scm")

(compile
  '(+ 1 2 3 4)
  'val
  'next)

;; a.

(define (spread-arguments args)
  (let ((args-length (length args)))
    (cond ((= args-length 0) (empty-instruction-sequence))
          ((= args-length 1) (compile (car args) 'arg1 'next))
          ((= args-length 2) (preserving
                               '(arg1 arg2)
                               (compile (car args) 'arg1 'next)
                               (compile (cadr args) 'arg2 'next)))
          (else (error "SPREAD-ARGUMENTS error args-length" args-length)))))

;; b.

(define (primitive-application? exp)
  (and (pair? exp)
       (> (length exp) 0)
       (memq (car exp) '(= + - / * > <))))

(define (compile-primitive-application exp target linkage)
  (let* ((op (car exp))
         (args (cdr exp))
         (args-length (length args))
         (apply-seq (cond ((= args-length 0)
                           (make-instruction-sequence
                             '(env) (list target)
                             `((assign ,target (op ,op)))))
                          ((= args-length 1)
                           (make-instruction-sequence
                             '(env) (list target)
                             `((assign ,target (op ,op) (reg arg1)))))
                          ((= args-length 2)
                           (make-instruction-sequence
                             '(env) (list target)
                             `((assign ,target (op ,op) (reg arg1) (reg arg2)))))
                          (else (error "COMPILE-PRIMITIVE-APPLICATION error"
                                       args-length args-length))
                          ))
         (spreaded-args-seq (spread-arguments args)))
    (preserving '(env proc) spreaded-args-seq apply-seq))))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ;;
        ((primitive-application? exp)
         (compile-primitive-application exp target linkage))
        ;;
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

;; c.

(define label-counter 0)

(format-compiled-code
  (compile
    '(define (factorial n)
       (define (iter product counter)
         (if (> counter n)
           product
           (iter (* counter product)
                 (+ counter 1))))
       (iter 1 1))
    'val
    'next))

; original version

((env)
 (val)
(
  (assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry7) (reg env))
  (goto (label after-lambda6))
entry7
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch22))
compiled-branch21
  (assign continue (label after-call20))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch22
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call20
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch9))
true-branch10
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))
false-branch9
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch16))
compiled-branch15
  (assign continue (label after-call14))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch16
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call14
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch13))
compiled-branch12
  (assign continue (label after-call11))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch13
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call11
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch19))
compiled-branch18
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch19
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call17
after-if8
after-lambda6
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch5))
compiled-branch4
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch5
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call3
after-lambda1
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
))

; Exercise 5.38 version

((env)
 (val)
(
  (assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry7) (reg env))
  (goto (label after-lambda6))
entry7
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
  (assign arg1 (op lookup-variable-value) (const counter) (reg env))
  (assign arg2 (op lookup-variable-value) (const n) (reg env))
  (assign val (op >) (reg arg1) (reg arg2))
  (test (op false?) (reg val))
  (branch (label false-branch9))
true-branch10
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))
false-branch9
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign arg1 (op lookup-variable-value) (const counter) (reg env))
  (assign arg2 (const 1))
  (assign val (op +) (reg arg1) (reg arg2))
  (assign argl (op list) (reg val))
  (assign arg1 (op lookup-variable-value) (const counter) (reg env))
  (assign arg2 (op lookup-variable-value) (const product) (reg env))
  (assign val (op *) (reg arg1) (reg arg2))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch13))
compiled-branch12
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch13
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call11
after-if8
after-lambda6
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch5))
compiled-branch4
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch5
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call3
after-lambda1
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
))

;; d.

(define (transform-primitive-application exp)
  (let* ((operator (car exp))
         (operands (cdr exp))
         (operands-length (length operands)))
    (if (<= operands-length 2)
      exp
      (let ((operand-1 (car operands))
            (operand-2 (cadr operands))
            (rest-operands (cddr operands)))
        (transform-primitive-application
          (append (list operator (list operator operand-1 operand-2))
                  rest-operands))))))

(transform-primitive-application '(+ 1 2 3 4)) ; (+ (+ (+ 1 2) 3) 4)

