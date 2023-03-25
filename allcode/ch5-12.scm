; The simulator can be used to help determine the data paths required for
; implementing a machine with a given controller. Extend the assembler to store
; the following information in the machine model:
;
; - a list of all instructions, with duplicates removed, sorted by instruction
; type (`assign`, `goto`, and so on);
; - a list (without duplicates) of the registers used to hold entry points
; (these are the registers referenced by `goto` instructions)
; - a list (without duplicates) of the registers that are `saved` or `restored`
; - for each register, a list (without duplicates) of the sources from which it
; is assigned (for example, the sources for register `val` in the factorial
; machine of Figure 5.11 are `(const 1)` and `((op *) (reg n) (reg val))`)
;
; Extend the message-passing interface to the machine to provide access to this
; new information. To test your analyzer, define the Fibonacci machine from
; Figure 5.12 and examine the lists you constructed.

(load "ch5-regsim.scm")

;; utility procedures
(define (inst-name inst)
  (if (pair? inst)
    (car inst)
    'label))

(define (goto? inst)
  (and (pair? inst)
       (eq? (car inst) 'goto)))

(define (save? inst)
  (and (pair? inst)
       (eq? (car inst) 'save)))

(define (assign? inst)
  (and (pair? inst)
       (eq? (car inst) 'assign)))

(define (restore? inst)
  (and (pair? inst)
       (eq? (car inst) 'restore)))

(define (register-name-in-save-or-restore inst)
  (cadr inst))

(define (register-name-in-goto inst)
  (cadadr inst))

(define (register-name-in-assign inst)
  (cadr inst))

(define (source-in-assign inst)
  (let ((expr (cddr inst)))
    (if (= 1 (length expr))
      (car expr)
      expr)))

; (define (source-in-assign inst)
;   (let ((expr (cddr inst)))
;     expr))

;; main modifications
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 5-12 part 1
    ((machine 'set-inst-names!)
     (map inst-name controller-text))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 5-12 part 2
    (let* ((goto-insts (filter goto? controller-text))
           (registers (map register-name-in-goto goto-insts)))   
      ((machine 'set-entry-point-registers!) registers))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 5-12 part 3
    (let* ((save-or-restore-insts
             (filter (lambda (inst)
                       (or (save? inst)
                           (restore? inst)))
                     controller-text))
           (register-names (map register-name-in-save-or-restore
                                save-or-restore-insts)))   
      ((machine 'set-saved-restored-registers!) register-names))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 5-12 part 4
    (let* ((assign-insts (filter assign? controller-text))
           (register-names (map register-name-in-assign
                                assign-insts))
           (sources (map source-in-assign assign-insts))
           (register-source-pairs (map (lambda (register-name source)
                                         (list register-name source))
                                       register-names
                                       sources)))
      ((machine 'set-register-to-sources!) register-source-pairs))
    machine))

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
            (list (list 'pc pc) (list 'flag flag)))
          (unique-op-names (list 'initialize-stack
                                 'print-stack-statistics)))
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
      ;; 5-12
      ; TODO: use `make-red-black-tree` instead of `make-equal-hash-table`
      ;       to satisfy the requirement that all the instructions should be
      ;       sorted
      (define inst-names (make-equal-hash-table))
      (define entry-point-registers (make-equal-hash-table))
      (define saved-restored-registers (make-equal-hash-table))
      (define register-to-sources (make-equal-hash-table))
      ;
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; 5-12 part 1
              ((eq? message 'set-inst-names!)
               (lambda (new-inst-names)
                 (for-each (lambda (inst-name)
                             (hash-table-set! inst-names inst-name #t))
                           new-inst-names)))
              ((eq? message 'inst-names) (hash-table-keys inst-names))
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; 5-12 part 2
              ((eq? message 'set-entry-point-registers!)
               (lambda (register-names)
                 (for-each (lambda (register-name)
                             (hash-table-set! entry-point-registers register-name #t))
                           register-names)))
              ((eq? message 'entry-point-registers) (hash-table-keys entry-point-registers))
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; 5-12 part 3
              ((eq? message 'saved-restored-registers) (hash-table-keys saved-restored-registers))
              ((eq? message 'set-saved-restored-registers!)
               (lambda (register-names)
                 (for-each (lambda (register-name)
                             (hash-table-set! saved-restored-registers register-name #t))
                           register-names)))
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; 5-12 part 4
              ((eq? message 'set-register-to-sources!)
               (lambda (register-source-pairs)
                 (for-each (lambda (register-source-pair)
                             (let* ((register-name (car register-source-pair))
                                    (source (cadr register-source-pair))
                                    (sources (hash-table-ref
                                               register-to-sources
                                               register-name
                                               (lambda () (make-equal-hash-table)))))
                               ; TODO: find a more elegant way to handle this
                               (hash-table-set! sources source #t)
                               (hash-table-set! register-to-sources register-name sources)))
                           register-source-pairs)))
              ; TODO: find a better way to deliver the values
              ((eq? message 'register-to-sources) register-to-sources)
              ;; end of 5-12
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define fib-machine
  (make-machine
    '(n continue val)
    (list
      (list '< <)
      (list '- -)
      (list '+ +)
      (list 'read read)
      )
    '(
      ;; hard code n's value for faster testing
      ; (assign n (op read))
      (assign n (const 10))
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
      afterfib-n-1                       ; upon return, val contains Fib(n-1)
      (restore n)
      (restore continue)
      ;; set up to compute Fib(n-2)
      (assign n (op -) (reg n) (const 2))
      (save continue)
      (assign continue (label afterfib-n-2))
      (save val)                         ; save Fib(n-1)
      (goto (label fib-loop))
      afterfib-n-2                       ; upon return, val contains Fib(n-2)
      (assign n (reg val))               ; n now contains Fib(n-2)
      (restore val)                      ; val now contains Fib(n-1)
      (restore continue)
      (assign val                        ; Fib(n-1)+Fib(n-2)
              (op +) (reg val) (reg n)) 
      (goto (reg continue))              ; return to caller, answer is in val
      immediate-answer
      (assign val (reg n))               ; base case: Fib(n)=n
      (goto (reg continue))
      fib-done)))

(start fib-machine)

(get-register-contents fib-machine 'val)

(fib-machine 'inst-names)

(fib-machine 'entry-point-registers)

(fib-machine 'saved-restored-registers)

(let* ((ht (fib-machine 'register-to-sources))
       (keys (hash-table-keys ht))
       (vals (hash-table-values ht))
       (vals-2 (map hash-table-keys vals)))
  (list (caddr keys)
        (caddr vals-2)))

