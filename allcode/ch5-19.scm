; Alyssa P. Hacker wants a *breakpoint* feature in the simulator to help her
; debug her machine designs. You have been hired to install this feature for
; her. She wants to be able to specify a place in the controller sequence where
; the simulator will stop and allow her to examine the state of the machine. You
; are to implement a procedure
;
;   (set-breakpoint <machine> <label> <n>)
;
; that sets a breakpoint just before the nth instruction after the given label.
; For example,
;
;   (set-breakpoint gcd-machine 'test-b 4)
;
; installs a breakpoint in `gcd-machine` just before the assignment to register
; `a`. When the simulator reaches the breakpoint, it should print the label and
; the offset of the breakpoint and stop executing instructions. Alyssa can then
; use `get-register-contents` and `set-register-contents!` to manipulate the
; state of the simulated machine. She should then be able to continue execution
; by saying
;
;   (proceed-machine <machine>)
;
; She should also be able to remove a specific breakpoint by means of
;
;   (cancel-breakpoint <machine> <label> <n>)
;
; or to remove all breakpoints by means of
;
;   (cancel-all-breakpoints <machine>)
;
; ---
;
; This exercise is like an upgraded version of 5.17: apart from adding the label
; information into the instructions, we also have to add an "offset number" to
; them. The reason is that `set-breakpoint` has these inputs:
;
; - `machine`
; - `label`
; - `n`
;
; And it
;
; > sets a breakpoint just before the nth instruction after the given label 
;
; The approach is to:
;
; - Save the breakpoints in `make-new-machine`
; - On every `execute`, check if the current instruction has a breakpoint set
;
; I can think of an alternative approach, which is to add a special instruction
; named `debug`, but it seems quite clunky in the sense that in case there are
; more than one debugging point, we have to take extra steps to make sure that
; the adding is correct. Apparently, the two approaches are called: hardware
; breakpoint, and software breakpoint.

(load "ch5-regsim.scm")

;; instruction structure procedures

(define (make-instruction label offset text)
  (list label
        offset
        text
        '()))

(define (instruction-label inst)
  (list-ref inst 0))

(define (instruction-offset inst)
  (list-ref inst 1))

(define (instruction-text inst)
  (list-ref inst 2))

(define (instruction-execution-proc inst)
  (list-ref inst 3))

(define (set-instruction-execution-proc! inst proc)
  (list-set! inst 3 proc))

;; register procedures

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
                   ; `write` is used instead of `display`
                   ; to handle the case when `contents` or `value` has circular
                   ; references (procedure `f` is stored as `(procedure f
                   ; [env])`, in which `env` contains `f` to allow recursion)
                   (display "Old value: ") (write contents)
                   (newline)
                   (display "New value: ") (write value)
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

;; breakpoint procedures

(define (make-breakpoint label offset)
  (list label offset))

(define (inst-should-break? breakpoints label offset)
  (member (make-breakpoint label offset)
          breakpoints))

;; assembling procedures

(define (assemble controller-text machine)
  (extract-labels
    0
    '*unassigned*
    controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

(define (extract-labels offset
                        current-label
                        text
                        receive)
  (if (null? text)
      (receive '() '())
      (let* ((next-inst (car text))
             (next-inst-label? (symbol? next-inst))
             (next-offset (if next-inst-label?
                            1
                            (+ offset 1)))
             (label (if next-inst-label?
                      next-inst
                      current-label)))
        (extract-labels
          next-offset
          label
          (cdr text)
          (lambda (insts labels)
            (if next-inst-label?
              (receive insts
                       (cons (make-label-entry label insts)
                             labels))
              (receive (cons (make-instruction label offset next-inst)
                             insts)
                       labels)))))))

;; utility procedures

(define (set-breakpoint machine label offset)
  ((machine 'set-breakpoint) label offset))

(define (proceed-machine machine)
  (machine 'proceed))

(define (cancel-breakpoint machine label offset)
  ((machine 'cancel-breakpoint) label offset))

(define (cancel-all-breakpoints machine)
  ((machine 'cancel-all-breakpoints)))

;; main procedures

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        ;; Exercise 5.15
        (instruction-count 0)
        ;; Exercise 5.16
        (trace #f)
        ;; Exercise 5.19
        (breakpoints '())
        )
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
      (define (execute force-exec-inst?)
        ;; `force-exec-inst?` is needed for Exercise 5.19, where a debugger is
        ;; added. Without anything to differentiate, `execute` is going to get
        ;; stuck at the breakpoint forever.
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              ;; Exercise 5.16
              (let* ((inst (car insts))
                     (inst-offset (instruction-offset inst))
                     (inst-label (instruction-label inst))
                     (inst-text (instruction-text inst))
                     (inst-proc (instruction-execution-proc inst)))
                ;; Exercise 5.15
                (set! instruction-count (+ 1 instruction-count))
                ;; Exercise 5.17
                (if trace
                  (begin
                    ;; Exercise 5.19
                    (display "Offset: ") (display inst-offset) (display "; ")
                    ;;
                    (display "Label: ") (display inst-label) (display "; ")
                    (display "Executing instruction: ") (display inst-text)
                    (newline)))
                ;; Exercise 5.19
                (if (and (inst-should-break? breakpoints
                                             inst-label
                                             inst-offset)
                         (not force-exec-inst?)) 
                  (begin
                    (display "Breakpoint reached. Stopped executing.")
                    (newline)
                    (set! the-instruction-sequence (get-contents pc))
                    (set-contents! pc '()))
                  (inst-proc))
                (execute #f)))))
              ;;
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute #f))
              ;; Exercise 5.19
              ((eq? message 'proceed)
               (set-contents! pc the-instruction-sequence)
               (execute #t))
              ;;
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ;; Exercise 5.15
              ((eq? message 'get-and-reset-instruction-count)
               (newline)
               (display "Instruction count: ") (display instruction-count)
               (set! instruction-count 0))
              ;; Exercise 5.16
              ((eq? message 'trace-on!) (set! trace #t))
              ((eq? message 'trace-off!) (set! trace #f))
              ((eq? message 'trace-on?) trace)
              ;; Exercise 5.18
              ((eq? message 'reg-tracing-on!)
               (lambda (reg-name)
                 (let ((reg (lookup-register reg-name)))
                   (reg 'tracing-on!))))
              ((eq? message 'reg-tracing-off!)
               (lambda (reg-name)
                 (let ((reg (lookup-register reg-name)))
                   (reg 'tracing-off!))))
              ;; Exercise 5.19
              ((eq? message 'set-breakpoint)
               (lambda (label offset)
                 (set! breakpoints (cons (make-breakpoint label offset)
                                         breakpoints))))
              ((eq? message 'cancel-breakpoint)
               (lambda (label offset)
                 (remove! (lambda (breakpoint)
                            (equal? breakpoint (make-breakpoint label offset)))
                          breakpoints)))
              ((eq? message 'cancel-all-breakpoints)
               (lambda ()
                 (set! breakpoints '())))
              ;;
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;; testing

;;FIGURE 5.11
(define fact-controller-text
  '(controller
      ;(assign n (op read))
      (assign n (const 4))
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
                      fact-controller-text))
      (proceed-on-break (lambda (fact-machine)
                          (display "Value of register 'val on debugger stop: ")
                          (display (get-register-contents fact-machine 'val))
                          (newline)
                          (proceed-machine fact-machine)
                          )))
  (fact-machine 'trace-on!)
  (set-breakpoint fact-machine 'after-fact 4)
  (start fact-machine)
  (proceed-on-break fact-machine)
  (proceed-on-break fact-machine)
  (proceed-on-break fact-machine)
  (get-register-contents fact-machine 'val))

(let* ((fib-controller-text
         '(controller
             ; (assign n (op read))
             (assign n (const 6))
             (assign continue (label fib-done))
           fib-loop
             (test (op <) (reg n) (const 2))
             (branch (label immediate-answer))
             ;; set up to compute Fib(n-1)
             (save continue)
             (assign continue (label afterfib-n-1))
             (save n)                           ; save old value of n
             (assign n (op -) (reg n) (const 1)); clobber n to n-1
             (goto (label fib-loop))            ; perform recursive call
           afterfib-n-1                         ; upon return, val contains Fib(n-1)
             (restore n)
             (restore continue)
             ;; set up to compute Fib(n-2)
             (assign n (op -) (reg n) (const 2))
             (save continue)
             (assign continue (label afterfib-n-2))
             (save val)                         ; save Fib(n-1)
             (goto (label fib-loop))
           afterfib-n-2                         ; upon return, val contains Fib(n-2)
             (assign n (reg val))               ; n now contains Fib(n-2)
             (restore val)                      ; val now contains Fib(n-1)
             (restore continue)
             (assign val                        ; Fib(n-1)+Fib(n-2)
                     (op +) (reg val) (reg n)) 
             (goto (reg continue))              ; return to caller, answer is in val
           immediate-answer
             (assign val (reg n))               ; base case: Fib(n)=n
             (goto (reg continue))
           fib-done))
       (fib-ops (list
                  (list 'read read)
                  (list '+ +)
                  (list '- -)
                  (list '< <)))
       (fib-registers '(n continue val))
       (fib-machine (make-machine fib-registers
                                  fib-ops
                                  fib-controller-text)))
  (fib-machine 'trace-on!)
  ((fib-machine 'reg-tracing-on!) 'val)
  (start fib-machine))

