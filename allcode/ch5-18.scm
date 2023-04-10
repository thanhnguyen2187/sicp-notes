; Modify the `make-register` procedure of Section 5.2.1 so that registers can be
; traced. Registers should accept messages that turn tracing on and off. When a
; register is traced, assigning a value to the register should print the name of
; the register, the old contents of the register, and the new contents being
; assigned. Extend the interface to the machine model to permit you to turn
; tracing on and off for designated machine registers.

(load "ch5-regsim.scm")

;; main procedures

(define (make-register name)
  (let ((contents '*unassigned*)
        (tracing 'off))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               ;; Exercies 5.18
               (if (eq? tracing 'on)
                 (begin
                   (display "Register name: ") (display name)
                   (newline)
                   (display "Old value: ") (display contents)
                   (newline)
                   (display "New value: ") (display value)
                   (newline)))
               ;;
               (set! contents value)))
            ((eq? message 'tracing-on!)
             (set! tracing 'on))
            ((eq? message 'tracing-off!)
             (set! tracing 'off))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ;; Exercise 5.18
              ((eq? message 'reg-tracing-on!)
               (lambda (reg-name)
                 (let ((reg (lookup-register reg-name)))
                   (reg 'tracing-on!))))
              ((eq? message 'reg-tracing-off!)
               (lambda (reg-name)
                 (let ((reg (lookup-register reg-name)))
                   (reg 'tracing-off!))))
              ;;
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))


;; testing

;;FIGURE 5.11
(define fact-controller-text
  '(controller
      (assign n (op read))
      (assign continue (label fact-done))     ; set up final return address
    fact-loop
      (test (op =) (reg n) (const 1))
      (branch (label base-case))
      ;; Set up for the recursive call by saving n and continue.
      ;; Set up continue so that the computation will continue
      ;; at after-fact when the subroutine returns.
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label fact-loop))
    after-fact
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val))   ; val now contains n(n-1)!
      (goto (reg continue))                   ; return to caller
    base-case
      (assign val (const 1))                  ; base case: 1!=1
      (goto (reg continue))                   ; return to caller
    fact-done))

(define fact-ops
  (list
    (list '= =)
    (list '- -)
    (list '* *)
    (list 'read read)))

(define fact-registers
  '(n continue val))

(let ((fact-machine (make-machine
                      fact-registers
                      fact-ops
                      fact-controller-text)))
  ((fact-machine 'reg-tracing-on!) 'val)
  ((fact-machine 'reg-tracing-on!) 'continue)
  (start fact-machine)
  )
