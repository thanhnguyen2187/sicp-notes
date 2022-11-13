; Use the simulator to test the machines you designed in Exercise 5.4.

(load "ch5-regsim.scm")

(define expt-machine
  (make-machine
    '(b n val continue)
    (list
      (list 'read read)
      (list '= =)
      (list '- -)
      (list '* *)
      )
    '(
      (assign b (op read))
      (assign n (op read))
      (assign val (const 1))

      expt-iter
      (test (op =) (const 0) (reg n))
      (branch (label expt-done))
      (assign val (op *) (reg val) (reg b))
      (assign n (op -) (reg n) (const 1))
      (goto (label expt-iter))

      expt-done)
    ))

(define expt-machine
  (make-machine
    '(b n val continue)
    (list
      (list 'read read)
      (list '= =)
      (list '- -)
      (list '* *)
      )
    '(
      (assign b (op read))
      (assign n (op read))
      (assign continue (label expt-done))

      expt-loop
      (test (op =) (reg n) (const 0))
      (branch (label expt-base-case))
      (save n)
      (save continue)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label expt-after))
      (goto (label expt-loop))

      expt-after
      (restore continue)
      (restore n)
      (assign val (op *) (reg b) (reg val))
      (goto (reg continue))

      expt-base-case
      (assign val (const 1))
      (goto (reg continue))

      expt-done)
    ))

(start expt-machine)

(get-register-contents expt-machine 'val)

