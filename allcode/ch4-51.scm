; Implement a new kind of assignment called `permanent-set!` that is not undone
; upon failure. For example, we can choose two distinct elements from a list and
; count the number of trials required to make a successful choice as follows:
;
; ...
;
; What values would have been displayed if we had used `set!` here rather than
; `permanent-set!`?
;
; ---
;
; `1` for `count` would have been displayed all the times.

(load "ch4-ambeval.scm")

;;(define the-global-environment (setup-environment))
(driver-loop)

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
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(restart 1)

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define count 0)
(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  ; (set! count (+ count 1))
  (permanent-set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))

try-again
