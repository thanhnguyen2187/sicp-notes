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
       (memq (car exp) '(= + - / *))))

(primitive-application? '(-))

(define (compile-primitive-application exp target linkage)
  (let* ((op (car exp))
         (args (cdr exp))
         (args-length (length args))
         (apply-seq (cond ((= args-length 0)
                           (make-instruction-sequence
                             '(env) target
                             `(assign ,target (op ,op))))
                          ((= args-length 1)
                           (make-instruction-sequence
                             '(env) target
                             `(assign ,target (op ,op) (reg arg1))))
                          ((= args-length 2)
                           (make-instruction-sequence
                             '(env) target
                             `(assign ,target (op ,op) (reg arg1) (reg arg2))))
                          (else (error "COMPILE-PRIMITIVE-APPLICATION error
                                       args-length" args-length))
                          ))
         (spreaded-args-seq (spread-arguments args)))
    (preserving '(env proc) spreaded-args-seq apply-seq))))

(compile-primitive-application
  '(+ 1 2)
  'val
  'next)

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
        ((application? exp)
         (compile-application exp target linkage))
        ((primitive-application? exp)
         (compile-primitive-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))


