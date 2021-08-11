; Exercise ?.?

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

(define (nested-interval n)
  (accumulate append
              nil
              (map (lambda (i)
                     (map (lambda (j) (list j i))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(define (filter predicate
                sequence)
  (flatmap (lambda (element)
             (if (predicate element)
                 (list element)
                 nil))
           sequence))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  ; For each item /x/ in /S/, recursively generate the sequence of permutations
  ; of /S - x/, and adjoin x to the front of each one.
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (prime? p)
  #t)

(define (prime-sum? pair)
  (prime? (+ (car pair)
             (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair)
           (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (nested-interval n))))

; (filter odd? (list 1 2 3 4 5 6))
(permutations (list 1 2 3))
