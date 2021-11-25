; Exercise 3.27
;
; *Memoization* (also called *tabulation*) is a technique that enables a
; procedure to record, in a local table, values that have previously been
; computed. This technique can make a vast difference in the performance of a
; program.
;
; A memorized procedure maintains a table in which values of previous calls are
; stored are using as keys for the the arguments that produced the values. When
; the memorized procedure is asked to compute a value, it first checks the table
; to see if the value is already there and, if so, just returns that value.
; Otherwise, it computes the new value in the ordinary way and stores this in
; the table. As an example of memoization, recall from Section 1.2.2 the
; exponential process for computing Fibonacci numbers:
;
; ...
;
; The memoized version of the same procedure is:
;
; ...
;
; where the memoizer is defined asked
;
; ...
;
; Draw an environment diagram to analyze the computation of `(memo-fib 3)`.
; Explain why `memo-fib` computes the /n^th/ Fibonacci number in a number of
; steps proportional to /n/. Would the scheme still work if we had simply
; defined `memo-fib` to be `(memoize fib)`?
;
; ---
;
; Global environment
; - make-table
; - lookup
; - insert!
; - memo-fib
; - memoize
;   - table
;     - x
;     - previously-computed-result
;       - result
;
; (memo-fib 3) -> (memoize) -> (memo-fib 2) -> (memoize) -> (memo-fib 1) -> (memoize) -> 1 -> (insert! 1 1 table)
;                              (memo-fib 1) -> (memoize) -> 1

#lang sicp

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(define (make-table) 0)
(define (lookup) 0)
(define (insert!) 0)

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

