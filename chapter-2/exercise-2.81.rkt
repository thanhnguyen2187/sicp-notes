; Exercise 2.81
;
; Louis Reasoner has noticed that `apply-generic` may try to coerce the
; arguments to each other's type even if they already have the same type.
; Therefore, he reasons, we need to put procedures in the coercion table to
; /coerce/ arguments of each type to their own type. 
;
; For example, in addition to the `scheme-number->complex` coercion shown above,
; he would do:
;
; ...
;
; a. With Louis's coercion procedures installed, what happens if `apply-generic`
; is called with two arguments of type `scheme-number` or two arguments of type
; `complex` for an operation that is not found in the table for those types? For
; example, assume that we've defined a generic exponentiation operation:
;
; ...
;
; and have put a procedure for exponentiation in the Scheme-number package, but
; not in any other package:
;
; ...
;
; What happens if we call `exp` with two complex numbers as arguments?
;
; b. Is Louis correct that something had to be done about coercion with
; arguments of the same type, or does `apply-generic` work correctly as is?
;
; c. Modify `apply-generic` so that it doesn't try coercion if the two arguments
; have the same type.
; 
; ---
;
; a. If we call `exp` with two complex numbers as arguments, the call would fall
; into an infinite loop, since complex number would get turns into complex
; number, again and again.
;
; b. Louis is not correct since in the original definition, we did have a list
; `(type type)`. Apply generic works correctly as it is.
;
; c.

#lang sicp

(define (square) 0)

(define (put) 0)
(define (get) 0)

(define (get-coercion) 0)
(define (put-coercion) 0)

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

(define (apply-generic-v2 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car  type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car  args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond ((eq? type1 type2) (apply proc (map contents args)))
                        (t1->t2 (apply-generic-v2 op (t1->t2 a1) a2))
                        (t2->t1 (apply-generic-v2 op a1 (t2->t1 a2)))
                        (else (error "No method for these types "
                                     (list op type-tags))))))
              (error "No method for these types "
                     (list op type-tags)))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)

  (define (tag x) (attach-tag 'scheme-number x))
  (define (exp x y) (apply-generic 'exp x y))

  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))

  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))

  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'is-zero?
       'scheme-number
       (lambda (x)
         (= 0 x)))

  'done)


(define (install-rectangular-package)

  ;; internal procedures

  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))

  (define (make-from-real-imag x y) (cons x y))

  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z)
          (real-part z)))

  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)

  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (x y) (tag (make-from-mag-ang x y))))


  'done)


(define (install-polar-package)

  ;; internal procedures

  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x)
                   (square y)))
          (atan y x)))

  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))

  (define (make-from-mag-ang r a)
    (cons r a))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))

  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle 'polar angle)

  (put 'make-from-real-imag 'polar
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)
         (tag (make-from-mag-ang r a))))

  'done)


(define (install-complex-package)

  ;; imported procedures from rectangular and polar packages

  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang x y)
    ((get 'make-from-mag-ang 'polar) x y))

  ;; internal procedures

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle     z1) (angle     z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle     z1) (angle     z2))))

  ;; interface to the rest of the system

  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-mag-ang x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'is-zero?
       'rectangular
       (lambda (x) (and (= 0 (real-part x))
                        (= 0 (imag-part x)))))

  'done)


(define (install-rational-package)

  ;; internal procedures

  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g)
            (/ d g))))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; interface to rest of the system

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d)
         (tag (make-rat n d))))

  (put 'is-zero?
       'rational
       (lambda (x) (and (= 0 (numer x))
                        (= 0 (denom x)))))

  'done)


(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle     z) (apply-generic 'angle     z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (scheme-number->complex n)
  (make-from-real-imag (contents n) 0))
(define (scheme-number->scheme-number n)
  n)
(define (complex->complex z)
  z)

(put-coercion 'scheme-number
              'complex
              scheme-number->complex)
(put-coercion 'scheme-number
              'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex
              'complex
              complex->complex)

(define z
  (apply (get 'make-from-real-imag 'complex)
         3 4))
