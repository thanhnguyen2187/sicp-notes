; Exercise 2.41
;
; Write a procedure to find all ordered triples of distinct positive integers
; /i/, /j/, and /k/ less than or equal to a given integer /n/ that sum to a
; given integer /s/.

#lang sicp

(define (enumerate-interval low
                            high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1)
                                    high))))

(define (accumulate operator
                    initial
                    sequence)
  (if (null? sequence)
      initial
      (operator (car sequence)
                (accumulate operator
                            initial
                            (cdr sequence)))))

(define (flatmap procedure
                 sequence)
  (accumulate append
              nil
              (map procedure
                   sequence)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list j i))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (filter predicate
                sequence)
  (flatmap (lambda (element)
             (if (predicate element)
                 (list element)
                 nil))
           sequence))

(define (find-triples n
                      s)
  (define (base p
                action)
    (let ((p1 (car p))
          (p2 (cadr p)))
      (let ((r (- s
                  p1
                  p2)))
        (action p1
                p2
                r))))
  (define (valid-pair p)
    (base p
          (lambda (p1 p2 r)
            (> r p2))))
  (define (prettify p)
    (base p
          (lambda (p1 p2 r)
            (list p1 p2 r))))
  (map prettify
       (filter valid-pair (unique-pairs n))))

(find-triples 18 11)
