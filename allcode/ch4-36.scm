;i Exercise 3.69 discussed how to generate the stream of *all* Pythagorean
; triples, with no upper bound on the size of the integers to be searched.
; Explain why simply replacing `an-integer-between` by
; `an-integer-starting-from` in the procedure in Exercise 4.35 is not an
; adequate way to generate arbitrary Pythagorean triples. Write a procedure that
; actually will accomplish this. (That is, write a procedure for which
; repeatedly typing `try-again` would in principle eventually generate all
; Pythagorean triples)
;
; ---
;
; Simply replacing `an-integer-between` by `an-integer-starting-from` is not an
; adequate way to generate arbitrary Pythagorean triples, since the algorithm
; for `amb` is a "naive" depth-first search that try another value for the
; second parameter only on the first value's failure.

(load "ch4-ambeval.scm")

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'memq memq)
        (list 'member member)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list '> >)
        (list '>= >=)
        (list 'abs abs)
        (list 'remainder remainder)
        (list 'integer? integer?)
        (list 'sqrt sqrt)
        (list 'eq? eq?)
;;      more primitives
        (list 'cadr cadr)
        (list 'caddr caddr)
        (list '< <)
        (list '<= <=)
        ))

;;(define the-global-environment (setup-environment))
(driver-loop)

(define (pair i j)
  (amb (list i j)
       (pair (+ j 1) i)))

try-again

(RESTART 1)

(define (require p)
  (if (not p) (amb)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between n m)
  (cond ((= n m) n)
        ((> n m) (amb))
        (else (amb n (an-integer-between (+ n 1) m)))))

(define (all-pythagorean-triple)
  (let ((k (an-integer-starting-from 1)))
    (let ((j (an-integer-between 1 k)))
      (let ((i (an-integer-between 1 j)))
        (require (= (+ (* i i)
                       (* j j))
                    (* k k)))
        (list i j k)))))

(all-pythagorean-triple)

try-again
