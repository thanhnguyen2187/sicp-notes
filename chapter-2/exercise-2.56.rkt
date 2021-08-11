; Exercise 2.56
;
; Show how to extend the basic differentiator to handle more kinds of
; expressions. For instance, implement the differentiation rule
;
; ...
;
; by adding a new clause to the `deriv` program and defining appropiate
; procedures `exponentiations?`, `base`, `exponent`, and `make-exponentiation`.
; (You may use the symbol ** to denote exponentiations.)
;
; Build in the rules that anything raised to the power of 0 is 1 and anything
; raised to the power of 1 is the thing itself.

#lang sicp

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
(define (make-sum a1 a2)
  ; Construct the sum of `a1` and `a2`.
  ;
  ; If both summands are numbers, `make-sum` will add them and return their sum.
  ; If one of the summand is 0, then `make-sum` will return the other summand.
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1)
              (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
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
(define (make-product m1 m2)
  ; Construct the product of `m1` and `m2`.
  ;
  ; 0 times anything is 0, and 1 times anything is the thing itself.
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1)
              (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))
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

(deriv '(** x 3) 'x)
