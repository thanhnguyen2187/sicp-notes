; The treatment of machine operations above permits them to operate on labels as
; well as on constants and the contents of registers. Modify the
; expression-processing procedures to enforce the condition that operations can
; be used only with registers and constants.

(load "ch5-regsim.scm")

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
          (map (lambda (e)
                 ;; TODO: handle the case when it is a register, but the content
                 ;;       of a register is a label
                 (if (or (constant-exp? e)
                         (register-exp? e))
                   (make-primitive-exp e machine labels)
                   (error "Invalid expression to be operated -- MAKE-OPERATION-EXP")))
               (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(pp cond)

(define test-op-machine
  (make-machine
    '(a b c t)
    (list
      (list 'read read)
      (list '= =)
      (list '- -)
      (list '+ +)
      (list '* *)
      )
    '(
      (assign a (const 3))
      (assign b (const 3))
      c
      (assign c (label c))
      (assign t (op +) (reg b) (reg a))
      )
    ))

(start test-op-machine)

(get-register-contents test-op-machine 't)

receive

