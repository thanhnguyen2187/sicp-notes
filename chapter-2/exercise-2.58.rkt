; Exercise 2.58
;
; Suppose we want to modify the differentiation program so that it works with
; ordinary mathematical notation, in which `+` and `*` are inflix rather than
; prefix operators. Since the differentation program is defined in terms of
; abstract data, we can modify it to work with different representations of
; expressions solely by chaning the predicates, selectors, and constructors that
; define the representation of the algebraic expressions on which the
; differentiator is to operate.
;
; a. Show how to do this in order to differentiate algebraic expressions
; presented in inflix form, such as `(x + (3 * (x + (y + 2))))`. To simplify the
; task, assume that `+` and `*` always take two arguments and that expressions
; are fully parenthesized.
;
; b. The problem becomes substantially harder if we allow standard algebraic
; notation, such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses
; and assumes that multiplication is done before addition. Can you design
; appropiate predicates, selectors, and constructors for this notation such that
; our derivative program sill works?

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

(define (zip sequence-1
             sequence-2)
  (if (or (null? sequence-1)
          (null? sequence-2))
      '()
      (cons (list (car sequence-1)
                  (car sequence-2))
            (zip  (cdr sequence-1)
                  (cdr sequence-2)))))

(define (join character sequence)
  (cond ((null? sequence) '())
        ((not (pair? sequence)) sequence)
        ((null? (cdr sequence)) (list (car sequence)))
        (else (append (list (car sequence) character)
                      (join character (cdr sequence))))))
(define (join-* sequence)
  (lambda (sequence) (join '* sequence)))
(define (join-+ sequence)
  (lambda (sequence) (join '+ sequence)))

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
(define (sign exp)
  (cadr exp))
(define (expression? exp)
  (pair? (car exp)))
(define (single-element? exp)
  (and (pair? exp)
       (null? (cdr exp))))

; Sum-related
(define (make-sum . A)
  ; Construct the sum of `a1`, `a2`, ..., `a_n` through A.
  ; (display A)
  (let ((split (filter-split number? A)))
    (let ((numbers (car split))
          (variables (cadr split)))
      (let ((numbers-sum (fold-left + 0 numbers)))
        (cond ((null? variables) numbers-sum)
              ((= 0 numbers-sum) (join-+ variables))
              (else (join-+ (append variables
                                    (list numbers-sum)))))))))

(define (sum? x)
  ; Is `x` a sum
  (and (pair? x)
       (not (single-element? x))
       (let ((x1 (car  x))
             (x2 (cadr x))
             (x3 (cddr x)))
         (or (eq? x2 '+)
             (sum? x3)))))

(define (addend x)
  ; Addend of the sum `x`.
  ; (display x) (newline)
  (if (single-element? x)
      (addend (car x))
      (let ((x1 (car  x))
            (x2 (cadr x))
            (x3 (cddr x)))
        (if (eq? x2 '+)
            x1
            (list x1 x2 (addend x3))))))

(define (augend x)
  ; Augend of the sum `x`.
  (if (single-element? x)
      (augend (car x))
      (let ((x1 (car  x))
            (x2 (cadr x))
            (x3 (cddr x)))
        (if (eq? x2 '+)
            x1
            (list x1 x2 (augend x3))))))

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
              ((= numbers-product 1) (join-* variables))
              (else (join-* (append (list numbers-product)
                                    variables))))))))

(define (product? x)
  ; Is `x` a product?
  (and (pair? x)
       (or (and (single-element? x)
                (product? (car x)))
           (let ((x1 (car  x))
                 (x2 (cadr x))
                 (x3 (cddr x)))
             ; (display x1) (newline)
             ; (display x2) (newline)
             ; (display x3) (newline)
             (and (eq? x2 '*)
                  (or (single-element? x3)
                      (product? x3)))))))

(define (multiplier x)
  ; Multiplier of the product `p`
  (if (single-element? x)
      (multiplier (car x))
      (let ((x1 (car  x))
            (x2 (cadr x))
            (x3 (cddr x)))
        (if (eq? x2 '*)
            x1
            (multiplier x3)))))

(define (multiplicand x)
  ; Multiplicand of the product `p`
  (if (single-element? x)
      (multiplicand (car x))
      (let ((x1 (car  x))
            (x2 (cadr x))
            (x3 (cddr x)))
        (if (eq? x2 '*)
            x3
            (multiplicand x3)))))

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
  ; (display exp) (newline)
  (cond ((number?   exp) 0)
        ((variable? exp)
                    (if (same-variable? exp
                                        var)
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

(define E '(x + (3 * (x + (y + 2)))))
; (deriv E 'x)
; (define E1 (addend E))
; (define E2 (augend E))
; (display E1) (newline)
; (display E2) (newline)
; 
; (product? E2)
; (multiplier E2)
; (multiplicand E2)

(deriv E 'x)

; (product? E)
; (sum? E)

; (deriv '(x * y) 'x)
; (join '+ (list 1 2 3 4 5))
; (make-product 'x 2 3 (make-sum 'x 'y 2 4 2 1) 'y)
; (define E1 '(x * 3 * y * 4 * z * 5))
; (define E2 '((x + 4) * (x + 5) * (x + 7)))
; (define E3 '((x * 7) * x * 5 + 3 + 4))
; (define E4 '(x + 3 * x + y + 2))
;
; (product? E1)
; (product? E2)
; (product? E3)
; (product? E4)
; 
; (newline)
; 
; (sum? E1)
; (sum? E2)
; (sum? E3)
; (sum? E4)
; 
; (multiplier   E1)
; (multiplicand E1)
; (multiplier   E2)
; (multiplicand E2)
; 
; (addend E3)
; (augend E3)
; (addend E4)
; (augend E4)
; (cdr '(1))
; (single-element? (list 1))

