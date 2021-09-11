; Exercise 3.9
;
; In section 1.2.1 we used the substitution model to analyze two procedures for
; computing factorials, a recursive version:
;
; ...
;
; and an iterative version
;
; ...
;
; Show the environment structures created by evaluating `factorial 6` using each
; version of the `factorial` procedure.

#lang sicp

(define (factorial-recursive n)
  (if (= n 1)
      1
      (* n (factorial-recursive (- n 1)))))

; (factorial-recursive 6)
; (* 6 (factorial-recursive 5))
; (* 6 (* 5 (factorial-recursive 4)))
; (* 6 (* 5 (* 4 (factorial-recursive 3))))
; (* 6 (* 5 (* 4 (* 3 (factorial-recursive 2)))))
; (* 6 (* 5 (* 4 (* 3 (* 2 (factorial-recursive 1))))))
; (* 6 (* 5 (* 4 (* 3 (* 2 1)))))
; (* 6 (* 5 (* 4 (* 3 2))))
; (* 6 (* 5 (* 4 6)))
; (* 6 (* 5 24))
; (* 6 120)
; 720

(define (factorial-iterative n)
  (define (iterate product counter max-count)
    (if (> counter max-count)
        product
        (iterate (* counter product)
                 (+ counter 1)
                 max-count)))
  (iterate 1 1 n))

; (factorial-iterative 6)
; (iterate 1   1 6)
; (iterate 1   2 6)
; (iterate 2   3 6)
; (iterate 6   4 6)
; (iterate 24  5 6)
; (iterate 120 6 6)
; (iterate 720 7 6)
; 720
