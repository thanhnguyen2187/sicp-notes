; Exercise 1.22
;
; Most Lisp implementations include a primitive called `runtime` that returns an
; integer specifies the amount of time the system has been running (measured,
; for example, in microseconds).
;
; The following `timed-prime-test` procedure, when called with an integer /n/,
; prints /n/ and checks to see if /n/ is prime. If /n/ is prime, the procedure
; prints three asterisks followed by the amount of time used in performing the
; test.
; 
; ...
;
; Using this procedure, write a procedure `search-for-primes` that checks the
; primality of consecutive odd integers in a specified range. Use your procedure
; to find the three smallest primes
; - larger than 1000;
; - larger than 10,000;
; - larger than 100,000;
; - larger than 1,000,000;
;
; Note the time needed to test each prime. Since the testing algorithms has
; order of growth of Omega(sqrt(n)), you should expect that testing for primes
; around 10,000 should take about sqrt(10) times as long as testing for primes
; around 1000.
;
; - Do your timing data bear this out?
; - How well do the data for 100,000 and 1,000,000 support Omega(sqrt(n))
; prediction?
; - Is your test result compatible with the notion that programs on your machine
; run in time proportional to the number of steps required for the computation?

#lang sicp

(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n
                    (- (runtime)
                       start-time))))
(define (report-prime
          n
          elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n)))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor
                                 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (square x) (* x x))

(define (search-for-primes lower upper)
  (define (iter n)
    (cond ((<= n upper)
           (timed-prime-test n)
           (iter (+ n 2)))))
  (iter (if (odd? lower)
            lower
            (+ lower 1))))

(search-for-primes 10000   10100)
(search-for-primes 100000  100100)
(search-for-primes 1000000 1000100)
