; Exercise 1.44
;
; The idea of /smoothing/ a function is an important concept in signal
; processing. If /f/ is a function and /dx/ is some small number, then the
; smoothed version of /f/ is the function whose value at a point /x/ is the
; average of /f(x - dx)/, /f(x)/ and /f(x + dx)/.
;
; Write a procedure `smooth` that takes as input a procedure that computes /f/
; and returns a procedure that computes the smoothed /f/.
;
; It is sometimes valuable to repeatly smooth a function (that is, smooth the
; smoothed function, and so on) to obtain the /n-fold smoothed function/.
;
; Show how to generate the /n/-fold smoothed function of any given function
; using `smooth` and `repeat` from Exercise 1.43.

#lang sicp

(define (average . xs)
  (define (iterate xs
                   counter
                   total)
    (if (null? xs)
        (/ total counter)
        (iterate (cdr xs)
                 (inc counter)
                 (+ total
                    (car xs)))))
  (iterate xs 0 0.0))

(define (repeat f
                times)
  (define (iterate x times)
    (if [= times 1]
        (f x)
        (iterate (f x)
                 (- times 1))))
  (lambda (x) (iterate x times)))

(define dx 0.001)
(define (smooth f)
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))
(define (n-fold-smooth f n)
  (repeat (smooth f) n))

(define smoothed-inc (n-fold-smooth inc 3))
(smoothed-inc 4)
