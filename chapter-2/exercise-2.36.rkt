; Exercise 2.36
;
; The procedure `accumulate-n` is similar to `accumulate` except that it takes
; as its third-argument a sequence of sequences, which are all assumed to have
; the same number of elements. It applies the designated accumulation process to
; combine all the first elements of the sequences, all the second elements of
; the sequences, and so on, and returns a sequence of the results.
;
; For instance, if `s` is a sequence containing four sequences `((1 2 3) (4 5 6)
; (7 8 9) (10 11 12))`, then the value of `(accumulate-n + 0)` should be the
; sequence `(22 26 30)`. Fill in the missing expressions in the following
; definition of `accumulate-n`:
;
; ...

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

(define sequences (list (list 1   2  3)
                        (list 4   5  6)
                        (list -1 -5  6)
                        (list 4   5  6)))
(accumulate-n + 0 sequences)
; (map inc 1)
