; Exercise ?.?

#lang sicp

(define (calculate initial
                   returns)
  (fold-right (lambda (return initial)
                (* initial (+ 1 return)))
              initial
              returns))

(define (fold-right operator
                    initial
                    sequence)
  (if (null? sequence)
      initial
      (operator (car sequence)
                (fold-right operator
                            initial
                            (cdr sequence)))))

(calculate 1
           (list 0.098
                 0.167
                 0.267
                 -0.1139
                 0.081
                 0.1608))

(calculate 1
           (list 0.067
                 0.09
                 0.099
                 0.0678
                 0.079
                 0.0567))

(calculate 1
           (list -0.131
                 -0.11
                 0.036
                 0.604
                 -0.071
                 0.0604
                 0.2295))
