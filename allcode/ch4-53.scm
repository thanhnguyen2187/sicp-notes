; With `permanent-set!` as described in Exercise 4.51 and `if-fail` as in
; Exercise 4.52, what will be the result of evaluating
;
; ...

(load "ch4-ambeval.scm")

; To be evaluated before the main loop

(define (if-fail? exp)
  (tagged-list? exp 'if-fail))

(define (if-fail-first-clause exp)
  (cadr exp))

(define (if-fail-second-clause exp)
  (caddr exp))

(define (analyze-if-fail exp)
  (let ((fproc (analyze (if-fail-first-clause exp)))
        (sproc (analyze (if-fail-second-clause exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (fproc-value fail2)
               (succeed fproc-value fail2))
             (lambda ()
               (sproc env
                      (lambda (sproc-value fail3)
                        (succeed sproc-value fail3))
                      fail))))))

(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))

(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok
                        fail2))
             fail))))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

;;(define the-global-environment (setup-environment))
(driver-loop)

(restart 1)

; To be evaluated after the main loop

(define (an-integer-between n m)
  (cond ((= n m) n)
        ((> n m) (amb))
        (else (amb n (an-integer-between (+ n 1) m)))))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (require p)
  (if (not p) (amb)))

(define (prime? x)
  (recurse 2))

(define (prime? x)
  (define (recurse index)
    (cond ((> index (sqrt x)) true)
          ((= 0 (remainder x index)) false)
          (else (recurse (+ index 1)))))
  (cond ((= 1 x) false)
        ((= 2 x) true)
        ((= 3 x) true)
        (else (recurse 2))))

(prime? 22)

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))

(an-element-of (list 1 2 3 4))

(prime-sum-pair '(1 3 5 8)
                '(20 35 110))

try-again

(let ((pairs '()))
  (if-fail (let ((p (prime-sum-pair '(1 3 5 8)
                                    '(20 35 110))))
             (permanent-set! pairs (cons p pairs))
             (amb))
           pairs))

; ((8 35) (3 110) (3 20))
