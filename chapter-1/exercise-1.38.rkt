; Exercise 1.38
;
; In 1737, the Swiss mathematican Leonhard Euler published a memoir
; /Defractionibus Continuis/, which included a continued fraction expansion for
; /e - 2/, where /e/ is the base of the natural logarithms. In this fraction,
; the /N_i/ are all 1, and the /D_i/ are successively 1, 2, 1, 1, 4, 1, 1, 6, 1,
; 1, 8, ...
;
; Write a program that uses your `cont-frac` procedure from Exercise 1.37 to
; approximate /e/, base on Euler's expansion.

#lang sicp

(define (cont-frac n
                   d
                   k)
  (define (recurse index)
    (if (= index k)
        (/ (n index)
           (d index))
        (/ (n index)
           (+ (d index)
              (recurse (+ index 1))))))
  (define (iterate index result)
    ; (display result)
    ; (newline)
    (if (= index 0)
        result
        (iterate (- index 1)
                 (/ (n index)
                    (+ result
                       (d index))))))
  (iterate k 0)
  ; (recurse 1)
  )

(+ 2
   (cont-frac (lambda (i) 1.0)
              (lambda (i)
                (if (= (remainder i
                                  3)
                       2)
                    (- i
                       (floor (/ i 3)))
                    1))
              996))
