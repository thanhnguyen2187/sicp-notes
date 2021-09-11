; Exercise 2.91
;
; A univariate polynomial can be divided by another one to produce a polynomial
; quotient and a polynomial remainder. For example,
;
; (x^5 - 1)/(x^2 - 1) = x^3 + 1, remainder x - 1.
;
; Division can be performed via long division.
;
; - That is, divide the highest-order term of the dividend by the highest-order
; term of the divisor.
; - Next, multiply the result by the divisor, subtract that from the dividend,
; and produce the rest of the answer by recursively dividing the difference
; by the divisor.
; - Stop when the order of the divisor exceeds the order of the dividend and
; declare the dividend to be the remainder.
; - Also, if the dividend ever becomes zero, return zero as both quotient and
; remainder.
;
; We can design a `div-poly` procedure on the model of `add-poly` and
; `mul-poly`.
;
; - The procedure checks to see if the two polys have the same variable.
; - If so, `div-poly` strips off the variable and passes the problem to
; `div-terms`, which performs the division operation on term lists.
; - `div-poly` finally reattaches the variable to the result supplied by
; `div-terms`.
;
; It is convenient to design `div-terms` to compute both the quotient and the
; remainder of a division. `div-terms` can take two term lists as arguments and
; return a list of the quotient term list and the remainder term list.
; 
; Complete the following definition of `div-terms` by filling in the missing
; expressions. Use this to implement `div-poly`, which takes two polys as
; arguments and returns a list of the quotient and remainder polys.

#lang sicp

(define (put) 0)
(define (get) 0)

(define (attach-tag type-tag contents)
  (cons (type-tag contents)))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG")))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS")))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC " (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-dense-package)

  ;; internal procedures

  (define the-empty-termlist 0)

  (define (make-poly variable term-list)
    (cons variable
          term-list))
  (define (variable  p) (car p))
  (define (term-list p) (cdr p))

  (define (=zero?) 0)

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (first-term      term-list) (car   term-list))
  (define (rest-terms      term-list) (cdr   term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car  term))
  (define (coeff term) (cadr term))

  (define (add-terms L1 L2) 0)
  (define (sub-terms L1 L2) 0)
  (define (mul-terms L1 L2) 0)

  (define (add-poly p1 p2) 0)
  (define (sub-poly p1 p2) 0)
  (define (mul-poly p1 p2) 0)

  (define (mul-term-by-all-terms) 0)

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (-   (order t1) (order t2))))
                (let ((rest-of-result
                        (div-terms (sub-terms L1
                                              (mul-term-by-all-terms
                                                (make-term new-o new-c) L2)))
                        ))
                  (list (adjoin-term (make-term new-o new-c)
                                     (car rest-of-result))
                        (cdr rest-of-result))
                  ))))))
  ;; interface to the rest of the system

  (define (tag p) (attach-tag 'dense p))

  (put 'make-poly 'dense
       (lambda (variable term-list) (tag (make-poly variable term-list))))

  (put 'add-poly  '(dense dense) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub-poly  '(dense dense) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul-poly  '(dense dense) (lambda (p1 p2) (tag (mul-poly p1 p2))))

  'done)

