; Implement a new construct called `if-fail` that permits the user to catch the
; failure of an expression. `if-fail` takes two expressions. It evaluates the
; first expression as usual and returns as usual if the evaluation succeeds. If
; the evaluation fails, however, the value of the second expression is returned,
; as in the following example:
;
; ...

(load "ch4-ambeval.scm")

;;(define the-global-environment (setup-environment))
(driver-loop)

(restart 1)

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

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

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
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

(define (even? x)
  (= (remainder x 2) 0))

(define (if-fail predicate alternative)
  (if predicate
      predicate
      alternative))

(an-element-of '(1 2 3))

(if-fail (let ((x (an-element-of '(1 3 5))))
           (require (even? x))
           x)
         'all-odd)

try-again

(define (analyze-if-fail exp)
  (let ((pproc ())
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail))))

