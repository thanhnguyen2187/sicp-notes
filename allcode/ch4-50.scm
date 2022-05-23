; Implement a new special form `ramb` that is like `amb` except that it searches
; alternatives in a random order, rather than from left to right. Show how can
; this help with Alyssa's problem in Exercise 4.49.

(load "ch4-ambeval.scm")

;;(define the-global-environment (setup-environment))
(driver-loop)

(ramb 1 2 3 4 5 6 7)

try-again

(restart 1)

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (pick-random-with-leftover sequence)
  (define (assemble left-half right-half)
    (cond ((null? left-half)  right-half)
          ((null? right-half) left-half)
          (else (append left-half
                        right-half))))
  (define (pick index left-half right-half)
    (cond ((null? right-half) (list '() '()))
          ((= index 0) (list (car right-half)
                             (assemble left-half
                                       (cdr right-half))))
          (else (pick (- index 1)
                      (append left-half
                              (list (car right-half)))
                      (cdr right-half)))))
  (if (null? sequence)
    (list '() '())
    (pick (random (length sequence))
          '()
          sequence)))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          (let* ((option (pick-random-with-leftover choices))
                 (choice (car option))
                 (other-choices (cadr option)))
            (choice env
                    succeed
                    (lambda ()
                      (try-next other-choices))))))
      (try-next cprocs))))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

