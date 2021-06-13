;

#lang sicp

(define (pascal-triangle row col)
  (cond ((or (= col 0)
             (> col row)) 0)
        ((or (= col 1)
             (= col row)) 1)
        (else (+ (pascal-triangle (- row 1) col)
                 (pascal-triangle (- row 1) (- col 1))))))

(pascal-triangle 1 1)
(pascal-triangle 2 1)
(pascal-triangle 2 2)
(pascal-triangle 3 2)
(pascal-triangle 4 3)
