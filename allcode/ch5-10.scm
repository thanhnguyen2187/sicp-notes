; Design a new syntax for register-machine instructions and modify the simulator
; to use your new syntax. Can you implement your new syntax without changing any
; part of the simulator except the syntax procedures in this section?

(load "ch5-regsim.scm")

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        ((eq? (car inst) 'inc)
         (make-inc inst machine pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(define (inc-reg-name inst)
  (cadr inst))

(define (make-inc inst machine pc)
  (let ((reg-name (inc-reg-name inst)))
    (lambda ()
      (set-register-contents!
        machine
        reg-name
        (+ (get-register-contents machine reg-name) 1))
      (advance-pc pc))))

(define inc-machine
  (make-machine
    '(x)
    '()
    '(
      (assign x (const 1))
      (inc x)
      (inc x)
      done)))

(start inc-machine)

(get-register-contents inc-machine 'x)

