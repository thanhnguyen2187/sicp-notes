(load "ch5-compiler.scm")
(load "format-compiled-code.scm")

;; lexical-address

(define (make-lexical-address
          frame-number
          displacement-number)
  (list frame-number displacement-number))

(define (lexical-frame-number address)
  (car address))

(define (lexical-placement-number address)
  (cadr address))

;; environment

(define (make-empty-env)
  '(()))

(define (make-env frame)
  (list frame))

(define (add-frame! frame env)
  (let ((old-env env))
    (set-car! env frame)
    (set-cdr! env old-env)
    'ok))

(define (add-frame frame env)
  (cons frame env))

(define (no-frame? env)
  (null? env))

(define (add-variable! variable compile-time-env)
  (let ((frame (first-frame compile-time-env)))
    (set-car! compile-time-env (cons variable frame))))

(define (add-value! value run-time-env)
  (let ((frame (first-frame run-time-env)))
    (set-car! run-time-env (cons value frame))))

;; defined in `ch5-eceval-support.scm`
;
; (define (first-frame env)
;   (car env))

(define (rest-frames env)
  (cdr env))

;; compile-time environment

(define (no-value? frame)
  (null? frame))

(define (first-value frame)
  (car frame))

(define (rest-values frame)
  (cdr frame))

(define (set-first-value! frame value)
  (set-car! frame value))

;; run-time environment

(define (no-variable? frame)
  (null? frame))

(define (first-variable frame)
  (car frame))

(define (rest-variables frame)
  (cdr frame))

;;

;; TODO: see if the style of many small utility functions does... obscure the
;;       understanding?

(define (lexical-address-lookup
          address
          run-time-env)
  (if (no-frame? run-time-env)
    '*unassigned*
    (let ((frame-number (lexical-frame-number address))
          (displacement-number (lexical-placement-number address))
          (frame (first-frame run-time-env)))
      (cond ((no-value? frame)
             '*unassigned*)
            ((and (= frame-number 0) (= displacement-number 0))
             (first-value frame))
            ((and (= frame-number 0) (> displacement-number 0))
             (lexical-address-lookup
               (make-lexical-address frame-number (- displacement-number 1))
               (make-env (rest-values frame))))
            ((> frame-number 0)
             (lexical-address-lookup
               (make-lexical-address (- frame-number 1) displacement-number)
               (rest-frames run-time-env)))))))

(lexical-address-lookup
  (make-lexical-address 0 0)
  '((1 2 3)
    (4 5 6))
  )
; 1

(lexical-address-lookup
  (make-lexical-address 1 1)
  '((1 2 3)
    (4 5 6))
  )
; 5

(lexical-address-lookup
  (make-lexical-address 3 0)
  '((1 2 3)
    (4 5 6))
  )
; *unassigned*

(define (lexical-address-set!
          address
          value
          run-time-env)
  (if (no-frame? run-time-env)
    (error "LEXICAL-ADDRESS-SET! received null run-time-env")
    (let ((frame-number (lexical-frame-number address))
          (displacement-number (lexical-placement-number address))
          (frame (first-frame run-time-env)))
      (cond ((no-value? frame)
             (error "LEXICAL-ADDRESS-SET! received null frame"))
            ((and (= frame-number 0) (= displacement-number 0))
             (begin
               (set-first-value! frame value)
               'ok))
            ((and (= frame-number 0) (> displacement-number 0))
             (lexical-address-set!
               (make-lexical-address frame-number (- displacement-number 1))
               value
               (make-env (rest-values frame))))
            ((> frame-number 0)
             (lexical-address-set!
               (make-lexical-address (- frame-number 1) displacement-number)
               value
               (rest-frames run-time-env)))))))

(let ((sample-env '((1 2 3) (4 5 6))))
  (lexical-address-set! (make-lexical-address 0 2)
                        0
                        sample-env)
  sample-env)
; ((1 2 0) (4 5 6))

(define (find-variable-in-frame variable frame)
  (define (iterate placement-number variable frame)
    (cond ((no-variable? frame)
           'not-found)
          ((eq? variable (first-variable frame))
           placement-number)
          (else (iterate (+ placement-number 1)
                         variable
                         (rest-variables frame)))))
  (iterate 0 variable frame))

(define (find-variable variable compile-time-env)
  (define (iterate frame-number variable compile-time-env)
    (if (no-frame? compile-time-env)
      'not-found
      (let* ((frame (first-frame compile-time-env))
             (found-displacement-number (find-variable-in-frame variable frame)))
        (if (eq? found-displacement-number 'not-found)
          (iterate (+ frame-number 1)
                   variable
                   (rest-frames compile-time-env))
          (make-lexical-address frame-number found-displacement-number)))))
  (iterate 0 variable compile-time-env))

(find-variable 'c '((y z) (a b c d e) (x y)))
; (1 2)

(find-variable 'x '((y z) (a b c d e) (x y)))
; (2 0)

(find-variable 'w '((y z) (a b c d e) (x y)))
; not-found

(define (global-lookup-variable-value var)
  (lookup-variable-value var (get-global-environment)))

;; modify "ch5-compiler.scm"

;; Exercise 5.40 and 5.42

(define (compile exp target linkage compile-time-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage compile-time-env))
        ((assignment? exp)
         (compile-assignment exp target linkage compile-time-env))
        ((definition? exp)
         (compile-definition exp target linkage compile-time-env))
        ((if? exp) (compile-if exp target linkage compile-time-env))
        ((lambda? exp) (compile-lambda exp
                                       target
                                       linkage
                                       compile-time-env))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
                           compile-time-env))
        ((cond? exp) (compile (cond->if exp)
                              target
                              linkage
                              compile-time-env))
        ((application? exp)
         (compile-application exp
                              target
                              linkage
                              compile-time-env))
        (else
          (error "Unknown expression type -- COMPILE" exp))))

(define (compile-variable exp target linkage compile-time-env)
  (let ((address (find-variable exp compile-time-env)))
    (end-with-linkage
      linkage
      (if (eq? address 'not-found)
        (make-instruction-sequence
          '() (list target)
          `((assign ,target
                    (op global-lookup-variable-value)
                    (const ,exp))))
        (make-instruction-sequence
          '(env) (list target)
          `((assign ,target
                    (op lexical-address-lookup)
                    (const ,address)
                    (reg env))))))))


(define (compile-assignment exp target linkage compile-time-env)
  (let* ((var (assignment-variable exp))
         (get-value-code (compile (assignment-value exp)
                                  'val
                                  'next
                                  compile-time-env))
         ; TODO: handle when `var-address` is 'not-found
         (var-address (find-variable var compile-time-env)))
    (end-with-linkage
      linkage
      (preserving
        '(env)
        get-value-code
        (make-instruction-sequence '(env val) (list target)
                                   `((perform (op lexical-address-set!)
                                              (const ,var-address)
                                              (reg val))
                                     (assign ,target (const ok))))))))

(define (compile-definition exp target linkage compile-time-env)
  (let ((var (definition-variable exp))
        (get-value-code (compile (definition-value exp)
                                 'val
                                 'next
                                 compile-time-env)))
    (add-variable! var compile-time-env)
    (end-with-linkage
      linkage
      (preserving
        '(env)
        get-value-code
        (make-instruction-sequence '(env val) (list target)
                                   `((perform (op add-value!)
                                              (reg val)
                                              (reg env))
                                     (assign ,target (const ok))))))))

(define (compile-if exp target linkage compile-time-env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage (if (eq? linkage 'next)
                                after-if
                                linkage)))
      (let ((p-code (compile (if-predicate exp)
                             'val
                             'next
                             compile-time-env))
            (c-code (compile (if-consequent exp)
                             target
                             consequent-linkage
                             compile-time-env))
            (a-code (compile (if-alternative exp)
                             target
                             linkage
                             compile-time-env)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                      (make-instruction-sequence '(val) '()
                                                 `((test (op false?) (reg val))
                                                   (branch (label ,f-branch))))
                      (parallel-instruction-sequences
                        (append-instruction-sequences t-branch c-code)
                        (append-instruction-sequences f-branch a-code))
                      after-if))))))

(define (compile-sequence seq target linkage compile-time-env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage compile-time-env)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next compile-time-env)
       (compile-sequence (rest-exps seq) target linkage compile-time-env))))

(define (compile-lambda exp target linkage compile-time-env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage (if (eq? linkage 'next)
                            after-lambda
                            linkage)))
      (append-instruction-sequences
        (tack-on-instruction-sequence
          (end-with-linkage
            lambda-linkage
            (make-instruction-sequence
              '(env) (list target)
              `((assign ,target
                        (op make-compiled-procedure)
                        (label ,proc-entry)
                        (reg env)))))
          (compile-lambda-body exp proc-entry compile-time-env))
        after-lambda))))

(define (compile-lambda-body exp proc-entry compile-time-env)
  (let* ((formals (lambda-parameters exp))
         (frame (first-frame compile-time-env))
         (new-compile-time-env (add-frame formals compile-time-env)))
    (append-instruction-sequences
      (make-instruction-sequence
        '(env proc argl) '(env)
        `(,proc-entry
           (assign env (op compiled-procedure-env) (reg proc))
           (assign env
                   (op add-frame)
                   (reg argl)
                   (reg env))))
      (compile-sequence (lambda-body exp)
                        'val
                        'return
                        new-compile-time-env))))

(define (compile-application exp target linkage compile-time-env)
  (let ((proc-code (compile (operator exp)
                            'proc
                            'next
                            compile-time-env))
        (operand-codes (map (lambda (operand) (compile operand
                                                       'val
                                                       'next
                                                       compile-time-env))
                            (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

;; Exercise 5.43

(define (filter-map pred func seq)
  (map func
       (filter pred seq)))

(define (invert pred)
  (lambda (arg)
    (not (pred arg))))

(define (to-assignment-exps define-exps)
  (let* ((variables (map definition-variable define-exps))
         (values (map definition-value define-exps))
         (variables-unassigned (map (lambda (variable)
                                      (list variable ''*unassigned*))
                                    variables))
         (variables-assignment (map (lambda (variable value)
                                      (list 'set! variable value))
                                    variables
                                    values)))
    ; variables
    (append (list 'let variables-unassigned)
            variables-assignment)
    ))

(to-assignment-exps '((define a 1) (define b 2)))

(define (scanning-out lambda-body)
  (let ((define-exps (filter definition? lambda-body))
        (other-exps (filter (invert definition?) lambda-body)))
    (append (to-assignment-exps define-exps)
            other-exps)))

(scanning-out '((define a 1) (define b 2) (+ a b)))

;;

;; Exercise 5.38
;; 
;; In theory, changing this definition in `ch5-38.scm` and adding
;; `compile-time-env` accordingly should work.

(define (primitive-application? exp compile-time-env)
  (let (operator (car exp))
    (and (pair? exp)
         (> (length exp) 0)
         (memq operator '(= + - / * > <))
         (eq? 'not-found (find-variable operator compile-time-env)))))

;; testing

(format-compiled-code
  (compile-variable 'a 'val 'next '((a))))

(format-compiled-code
  (compile-variable '+ 'val 'next '((a))))

(format-compiled-code
  (compile-assignment '(set! d 10) 'val 'next '((a b c) (d e f))))

(format-compiled-code
  (compile-definition '(define x 5) 'val 'next '(())))

(format-compiled-code
  (compile-definition '(define x (+ 2 3)) 'val 'next '(())))

(format-compiled-code
  (compile-sequence '(begin 1 2 3) 'val 'next '(())))

(format-compiled-code
  (compile-lambda '(lambda (x) (+ x 1)) 'val 'next '(())))

(format-compiled-code
  (compile '((lambda (x y)
               (lambda (a b c d e)
                 ((lambda (y z) (* x y z))
                  (* a b x)
                  (+ c d x))))
             3 4)
           'val
           'next
           '(())))

(format-compiled-code
  (compile-definition '(define (f x) (+ x 1)) 'val 'next '(())))

(format-compiled-code
  (compile-definition '(let ((x 1) (y 2)) (+ x y)) 'val 'next '(())))
