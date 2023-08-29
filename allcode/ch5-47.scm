; This section described how to modify the explicit-control evaluator so that
; interpreted code can call compiled procedures.
;
; Show how to modify the compiler so that compiled procedures can call not only
;
; - primitive procedures and
; - compiled procedures, but
; - interpreted procedures
;
; as well.
;
; This requires modifying `compile-procedure-call` to handle the case of
; compound (interpreted) procedures.
;
; Be sure to handle all the same `target` and `linkage` combinations as in
; `compile-proc-appl`.
;
; To do the actual procedure application, the code needs to jump to the
; evaluator's `compound-apply` entry point. This label cannot be directly
; referenced in object code (since the assembler requires that all labels
; referenced by the code it is assembling be defined there), so we will add a
; register called `compapp` to the evaluator machine to hold this entry point,
; and add an instruction to initialize it:
;
; ...
;
; To test your code, start by defining a procedure `f` that calls a procedure
; `g`. Use `compile-and-go` to compile the definition of `f` and start the
; evaluator. Now, typing at the evaluator, define `g` and try to call `f`.

(load "ch5-compiler")
(load "load-eceval-compiler")
(load "format-compiled-code")

(define (compile-compound-proc-appl target linkage)
  (cond ((and (not (eq? linkage 'return))
              (eq? target 'val))
         (make-instruction-sequence '(proc) all-regs
                                    `((assign continue (label ,linkage))
                                      (assign val (op compiled-procedure-entry) (reg proc))
                                      (goto (reg val)))))
        ((and (not (eq? linkage 'return))
              (not (eq? target 'val)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
                                      `((assign continue (label ,proc-return))
                                        (assign val (op compiled-procedure-entry) (reg proc))
                                        (goto (reg val))
                                        ,proc-return
                                        (assign ,target (reg val))
                                        (goto (label ,linkage))))))
        ((and (eq? linkage 'return)
              (eq? target 'val))
         (make-instruction-sequence '(proc continue) all-regs
                                    '((assign val (op compiled-procedure-entry) (reg proc))
                                      (goto (reg val)))))
        ((and (eq? linkage 'return)
              (not (eq? target 'val)))
         (error "return linkage, target not val -- COMPILE"
                target))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        ;; Exercise 5.47
        (compound-branch (make-label 'compound-branch))
        ;;
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage (if (eq? linkage 'next)
                              after-call
                              linkage)))
      (append-instruction-sequences
        (make-instruction-sequence '(proc) '()
                                   `((test (op primitive-procedure?) (reg proc))
                                     (branch (label ,primitive-branch))))
        ;; Exercise 5.47
        (make-instruction-sequence '(proc) '()
                                   `((test (op compound-procedure?) (reg proc))
                                     (branch (label ,compound-branch))))
        ;;
        (parallel-instruction-sequences
          (parallel-instruction-sequences
            (append-instruction-sequences
              compiled-branch
              (compile-proc-appl target compiled-linkage))
            (append-instruction-sequences
              primitive-branch
              (end-with-linkage
                linkage
                (make-instruction-sequence
                  '(proc argl)
                  (list target)
                  `((assign ,target (op apply-primitive-procedure) (reg proc) (reg argl)))))))
          (append-instruction-sequences
            compound-branch
            (compile-proc-appl target compiled-linkage)
            ))
        after-call))))

(define f-code
  '(define (f)
    (+ 10 (g))))

(define label-counter 0)
(compile-and-go f-code)

(define (g)
  5)

(f)

