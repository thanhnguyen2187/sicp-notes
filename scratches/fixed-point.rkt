;

#lang sicp

(define tolerance 0.00001)
(define (fixed-point f
                     first-guess)

  (define (close-enough? value-1 value-2)
    (< (abs (- value-1
               value-2))
       tolerance))

  (define (try guess)
    (let ((next (f guess)))
      (if [close-enough? guess next]
          next
          (try next))))

  (try first-guess))

;; (fixed-point cos 1.0)
;; (fixed-point (lambda (y)
;;                (+ (sin y)
;;                   (cos y)))
;;              1.0)
(define (average a b)
  (/ (+ a b)
     2))
(define (average-damp f)
  (lambda (x) (average x
                       (f x))))
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx))
                    (g x))
                 dx)))

; If x |-> g(x) is a differentiable function, then a solution to the equation
; /g(x) = 0 is a fixed point of the function x |-> f(x) where
;
; f(x) = x - g(x)/Dg(x)

(define (newton-transform g)
  (lambda (x) (- x
                 (/ (g x)))))
(define (newtons-method g
                        guess)
  (fixed-point (newton-transform g)
               guess))

(define (cube x) (* x x x))
((deriv cube) 5)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x
                                            (* y y))))
               1.0))

(cube-root 6)
(cube-root 7)
(cube-root 8)

(define (fixed-point-of-transform g
                                  transform
                                  guess)
  (fixed-point (transform g)
               guess))
(define (sqrt-v2 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

; (sqrt 3)
