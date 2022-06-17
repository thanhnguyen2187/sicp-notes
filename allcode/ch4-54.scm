; If we had not realized that `require` could be implemented as an ordinary
; procedure that uses `amb`, to be defined by the user as part of a
; nondeterministic program, we would have had to implement it as a special form.
; This would require syntax procedures:
;
; ...
;
; and a new clause in the dispatch in `analyze`
;
; ...
;
; as well as the procedure `analyze-require` that handles `require` expressions.
; Complete the following definition of `analyze-require`:
;
; ...

(load "ch4-ambeval.scm")

;;(define the-global-environment (setup-environment))
(driver-loop)

(restart 1)

(define (require? exp)
  (tagged-list? exp 'require))

(define (require-predicate exp)
  (cadr exp))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((require? exp) (analyze-require exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))


(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not pred-value)
                 (fail2)
                 (succeed 'ok fail2)))
             fail))))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(an-element-of (list 1 2 3 4 5))

try-again

