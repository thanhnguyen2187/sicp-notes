; Exercise 2.38
;
; The `accumulate` procedure is also known as `fold-right`, because it combines
; the first element of the sequence with result of combining all the elements to
; the right. There is also `fold-left`, which is similar to `fold-right`, except
; that it combines elements working in the opposite direction:
;
; ...
;
; What are the values of
;
; ...
;
; Give a property that `operator` should satisfy to guarantee that `fold-right`
; and `fold-left` will produce the same values for any sequence.

#lang sicp

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

(define (fold-right operator
                    initial
                    sequence)
  (if (null? sequence)
      initial
      (operator (car sequence)
                (fold-right operator
                            initial
                            (cdr sequence)))))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

; The property is associative.