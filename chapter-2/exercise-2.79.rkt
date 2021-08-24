; Exercise 2.79
;
; Define a generic equality predicate `equ?` that tests the equality of two
; numbers, and install it in the generic arithmetic package. This operation
; should work for ordinary numbers, rational numbers, and complex numbers.

#lang sicp

(define (all predicate . xs)
  ; (display xs) (newline)
  (if (null? xs)
      true
      (and (predicate (car xs))
           (apply all (cons predicate
                            (cdr xs))))))

(define (put) 0)
(define (get) 0)
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC " (list op type-tags))))))

(define (attach-tag type-tag contents) 
  (if (number? contents)
      contents
      (cons (type-tag contents))))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) datum)
        (error "Bad tagged datum: TYPE-TAG")))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (error "Bad tagged datum: CONTENTS")))

(define (tagged? symbol x)
  (eq? (type-tag x) symbol))
(define (complex-tagged? x)
  (lambda (x) (tagged? 'complex x)))
(define (rational-tagged? x)
  (lambda (x) (tagged? 'rational x)))

(define (equ? x y)
  (cond ((all number?          x y) (= x y))
        ((all complex-tagged?  x y) (apply-generic 'equal? x y))
        ((all rational-tagged? x y) (apply-generic 'equal? x y))
        (else (error "Invalid input: EQU? " x y))))

(put 'equal? '(complex-number complex-number)
     (lambda (x y) (and (= (apply-generic 'real-part x)
                           (apply-generic 'real-part y))
                        (= (apply-generic 'imag-part x)
                           (apply-generic 'imag-part y)))))
(put 'equal? '(rational-number rational-number)
     (lambda (x y) (and (= (apply-generic 'real-part x)
                           (apply-generic 'real-part y))
                        (= (apply-generic 'imag-part x)
                           (apply-generic 'imag-part y)))))

(all odd? 1 3 5 7 9)
(all odd? 1)
(all odd? 1 2 3 4 5)

(eq? 'one 'one)
