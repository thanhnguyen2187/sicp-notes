; Exercise 1.33
;
; You can obtain an even more general version of `accumulate` by introducing the
; notion of a /filter/ on the terms to be combined. That is, combine only those
; terms derived from values in the range that satisfy a specified condition. The
; result `filterted-accumulate` abstraction takes the same argments as
; `accumulate`, together with an additional predicate of one argument that
; specifies the filter.
;
; Write `filterted-accumulate` as a procedure. Show how to express the following
; using `filterted-accumulate`:
;
; a. the sum of the squares of the prime numbers in the interval /a/ to /b/
; (assuming that you have a prime? predicate already written)
;
; b. the product of all the positive integers less than /n/ that are relatively
; prime to /n/ (i.e., all positive integers /i < n/ such that /GCD(i, n) = 1/).

#lang sicp

(define (accumulate-filter combiner
                           null-value
                           term
                           a
                           next
                           b
                           predicate)
  (define (iter a result)
    ; (display a '\n)
    (if [> a b]
        result
        (iter (next a)
              (combiner (if (predicate a)
                            (term a)
                            null-value)
                        result))))
  (iter a null-value))

(define (product-relative-primes n)
  (define (relative-prime? m)
    (= (gcd m n)
       1))
  (define (skip m) #t)
  (accumulate-filter *
                     1
                     identity
                     1
                     inc
                     n
                     relative-prime?))

(product-relative-primes 11)
