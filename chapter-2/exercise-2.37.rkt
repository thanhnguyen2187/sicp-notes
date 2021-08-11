; Exercise 2.37
;
; Suppose we represent vectors /v = (v_i)/ as sequences of numbers, and matrices
; /m = (m_ij)/ as sequences of vectors (the rows of the matrix). For example,
; the matrix
;
; ...
;
; is represented as the sequence ((1 2 3 4) (5 6 7 8) (9 10 11 12)).
;
; With this representation, we can use sequence operations to concisely express
; the basic matrix and vector operations. These operations (which are described
; in any book on matrix algebra) are the following:
;
; ...
;
; We can define the dot product as
;
; ...
;
; Fill in the missing expressions in the following procedures for comuting the
; other matrix operations. (The procedure `accumulate-n` is defined in Exercise
; 2.36.)

#lang sicp

(define ? 0)
(define (accumulate operator
                    initial
                    sequence)
  (if (null? sequence)
      initial
      (operator (car sequence)
                (accumulate operator
                            initial
                            (cdr sequence)))))

(define (accumulate-n operator
                      initial
                      sequences)
  (if (null? (car sequences))
      nil
      (cons (accumulate operator
                        initial
                        ; accumulate the first elements of the sequences
                        (map car sequences))
            (accumulate-n operator
                          initial
                          (map cdr sequences)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w))
       m))

(define (transpose m)
  (accumulate-n cons nil m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v)
           (matrix-*-vector cols v))
         m)))

(define M1 (list (list 1 2 3)
                 (list 4 5 6)
                 (list 7 8 9)))
(define M2 (list (list 1 2)
                 (list 3 4)
                 (list 5 6)))
(define M3 (list (list 1 2 3)
                 (list 4 5 6)
                 (list 7 8 9)))
(define I  (list (list 1 0 0)
                 (list 0 1 0)
                 (list 0 0 1)))

(define V1 (list 3 3 3))
(define V2 (list -1 -1))

; (matrix-*-vector M1 V1)
; (matrix-*-vector M2 V2)
; (transpose M1)
; (transpose M2)
(transpose (transpose M2))
(matrix-*-matrix I M1)
