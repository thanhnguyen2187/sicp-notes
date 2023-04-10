; Extend the instruction tracing of Exercise 5.16 so that before printing an
; instruction, the simulator prints any labels that immediately precede that
; instruction in the controller sequence. Be careful to do this in a way that
; does not interfere with instruction counting (Exercise 5.15). You will have to
; make the simulator retain the necessary label information.
;
; ---
;
; The exercise is quite challenging, as putting a `current-label` inside
; `make-new-machine` will not work as expected.

(load "ch5-regsim.scm")

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
        ;;
        (current-label '*unassigned*)
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
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              ;; Exercise 5.16
              (let* ((inst (car insts))
                     (inst-text (instruction-text inst))
                     (inst-proc (instruction-execution-proc inst)))
                ;; Exercise 5.15
                (set! instruction-count (+ 1 instruction-count))
                ;;
                (if trace
                  (begin
                    (display "Label: ")
                    (display current-label)
                    (display "; ")
                    (display "Executing instruction: ")
                    (display inst-text)
                    (newline)))
                ;; Exercise 5.17
                (let ((inst-name (car inst-text)))
                  (if (or (eq? inst-name 'branch)
                          (eq? inst-name 'goto))
                    (let ((dest (cadr inst-text)))
                      ; fetch the destination from something like
                      ;
                      ;   (goto (reg continue))
                      ;   ; or
                      ;   (branch (label base-case))
                      (cond ((label-exp? dest)
                             (set! current-label (label-exp-label dest)))
                            ((register-exp? dest)
                             ; TODO: find another way to handle correctly handle
                             ;       this since `register-exp-reg` only returns
                             ;       `(reg continue)`, and
                             ;       `(get-contents (lookup-register (register-exp-reg dest)))`
                             ;       does not actually return the label's name,
                             ;       but return a list of instructions instead
                             (set! current-label dest)
                             )
                            (else (error "Unreachable code -- MAKE-NEW-MACHINE EXECUTE " inst-text))))))
                ;;
                (inst-proc)
                (execute)))))
              ;;
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
              ;; Exercise 5.15
              ((eq? message 'get-and-reset-instruction-count)
               (newline)
               (display "Instruction count: ") (display instruction-count)
               (set! instruction-count 0))
              ;; Exercise 5.16
              ((eq? message 'trace-on!) (set! trace #t))
              ((eq? message 'trace-off!) (set! trace #f))
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
  (fact-machine 'trace-on!)
  (start fact-machine)
  )
