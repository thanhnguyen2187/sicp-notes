; Exercise 2.57
;
; Extend the differentiation program to handle sums and products of arbitrary
; numbers of (two or more) terms. Then the last example above could be expressed
; as 
;
; ...
;
; Try doing this by changing only the representation of for sums and products,
; without changing the `deriv` procedure at all. For example, the `addend` of a
; sum would be the first term, and the `augend` would be the sum of the rest of
; the terms.

#lang sicp

; Utilities
(define (accumulate operator
                    initial
                    sequence)
  (if (null? sequence)
      initial
      (operator (car sequence)
                (accumulate operator
                            initial
                            (cdr sequence)))))

(define (fold-left operator
                   initial
                   sequence)
  (define (iterate result
                   rest)
    (if (null? rest)
        result
        (iterate (operator result (car rest))
                 (cdr rest))))
  (iterate initial
           sequence))

(define (flatmap procedure
                 initial
                 sequence)
  (accumulate append
              initial
              (map procedure
                   sequence)))

(define (filter predicate
                sequence)
  (flatmap (lambda (element)
             (if (predicate element)
                 (list element)
                 nil))
           '()
           sequence))

(define (filter-split predicate
                      sequence)
  ; use a predicate to filter the sequence, and then split the result into two
  ; sequences
  ;
  ;   (filter-split odd? (list 1 2 3 4 5 6))
  ;   -> ((1 3 5) (2 4 6))
  (fold-left (lambda (result element)
               (let ((sequence-1 (car  result))
                     (sequence-2 (cadr result)))
                 (if (predicate element)
                     (list (append sequence-1
                                   (list element))
                           sequence-2)
                     (list sequence-1
                           (append sequence-2
                                   (list element))))))
             (list (list)
                   (list))
             sequence))

; Primitives
(define (variable? x)
  ; Is `x` a variable?
  (symbol? x))

(define (same-variable? v1 v2)
  ; Are v1 and v2 the same variable?
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  ; Checks whether an expression is equal to a given number
  (and (number? exp)
       (= exp num)))

; Sum-related
(define (make-sum . A)
  ; Construct the sum of `a1`, `a2`, ..., `a_n` through A.
  ; (display A)
  (let ((split (filter-split number? A)))
    (let ((numbers (car split))
          (variables (cadr split)))
      (let ((numbers-sum (fold-left + 0 numbers)))
        (cond ((null? variables) numbers-sum)
              ((= 0 numbers-sum) variables)
              (else (append (list '+ numbers-sum)
                            variables))
            )))))

(define (sum? x)
  ; Is `x` a sum
  (and (pair? x)
       (eq? (car x) '+)))

(define (addend s)
  ; Addend of the sum `s`.
  (cadr s))
(define (augend s)
  ; Augend of the sum `s`.
  (caddr s))

; Product-related
(define (make-product . P)
  ; Construct the product of `m1` and `m2`.
  ;
  ; 0 times anything is 0, and 1 times anything is the thing itself.
  (let ((split (filter-split number? P)))
    (let ((numbers   (car  split))
          (variables (cadr split)))
      (let ((numbers-product (fold-left * 1 numbers)))
        (cond ((= numbers-product 0) 0)
              ((= numbers-product 1) variables)
              (else (append (list '* numbers-product)
                            variables))))))
  )
(define (product? x)
  ; Is e a product?
  (and (pair? x)
       (eq? (car x) '*)))

(define (multiplier p)
  ; Multiplier of the product `p`
  (cadr p))
(define (multiplicand p)
  ; Multiplicand of the product `p`
  (caddr p))

(define (square x) (* x x))

(define (** a n)
  (cond ((= n 0) 1)
        ((= n 1) a)
        ((even? n) (square (** a
                               (/ n 2))))
        ((odd? n) (* a (square (** a
                                   (/ (- n 1) 2)))))))

; Exponentiation related
(define (make-exponentiation a n)
  (cond ((and (number? a)
              (number? n)) (** a n))
        (else (list '** a n))))
(define (exponentiations? x)
  (and (pair? x)
       (eq? (car x)
            '**)))

(define (base x)
  ; Base of the exponentiation `x`.
  (cadr x))
(define (exponent x)
  ; Exponent of exponentiation `x`.
  (caddr x))

; Derivation-related
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
                    (if (same-variable? exp var)
                        1
                        0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp)
                                                (deriv (multiplicand exp) var))
                                  (make-product (deriv (multiplier exp) var)
                                                (multiplicand exp))))
        ((exponentiations? exp)
         (let ((a (base exp))
               (n (exponent exp)))
           (make-product n
                         (make-exponentiation a (- n 1)))))
        (else (error "unknown expression type: DERIV" exp))))

; (define L (filter-split odd? (list 1 2 3 4 5 6)))
; (car L)
; (cadr L)
(make-sum 'x 0)
(make-product 'x 0 20)
