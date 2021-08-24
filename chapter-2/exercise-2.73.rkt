; Exercise 2.73
;
; Section 2.3.2 described a program that performs symbolic differentiation:
;
; ...
;
; a. Explain what was done above. Why can't we assimilate the predicates
; `number?` and `variable?` into the data-directed dispatch?
; b. Write the procedures for derivatives of sums and products, and the auxilary
; code required to install them in the table used by the program above.
; c. Choose any additional differentiation rule that you like, such as the one
; for exponents (Exercise 2.56), and install it in this data-directed system.
; d. In this simple algebraic manipulator the type of an expression is the
; algebraic operator that binds it together. Suppose, however, we indexed the
; procedures in the opposite way, so that the dispatch line in `deriv` looked
; like
;
; ...
;
; What corresponding changes to the derivative system are required?
;
; ---
;
; a. We cannot do it because of the mismatching interface between a
; number/variable, and a list.
; b. ...
; c. ...
; d. We need to change the position of `'deriv` and `'[operator]`
;

#lang sicp

; (define (deriv exp var)
;   (cond ((number? exp) 0)
;         ((variable? exp)
;          (if (same-variable? exp var)
;              1
;              0))
;         ((product? exp)
;          (make-sum (make-product (multiplier exp)
;                                  (deriv (multiplicand exp) var))
;                    (make-product (deriv (multiplier exp) var)
;                                  (multiplicand exp))))
;         (else (error "unknown expression type: DERIV " exp))))

(define (variable?) 0)
(define (same-variable?) 0)
(define (attach-tag) 0)
(define (get) 0)
(define (put op type item)
  ;; Installs the `item` in the table, indexed by `op` and `get`, for
  ;; manipulating the operation and type table.
  0)

(define (make-sum) 0)
(define (make-product) 0)
(define (make-exponentation) 0)

(define (install-deriv)
  (define (tag x) (attach-tag 'deriv x))
  (put 'deriv '+ (lambda (operands var)
                   (let ((addend (car  operands))
                         (augend (cadr operands)))
                     (make-sum (deriv addend var)
                               (deriv augend var)))))
  (put 'deriv '* (lambda (operands var)
                   (let ((multiplier   (car  operands))
                         (multiplicant (cadr operands)))
                     (make-sum (make-product (car operands)
                                             (deriv (cadr operands) var))
                               (make-product (deriv (car operands) var)
                                             (cadr operands))))))
  (put 'deriv '** (lambda (operands var)
                    (let ((base (car operands))
                          (exponent (cadr operands)))
                      (make-product exponent
                                    (make-exponentation base
                                                        (- exponent - 1))))))
  'done)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
; ((get (operator exp) 'deriv) (operands exp) var)
