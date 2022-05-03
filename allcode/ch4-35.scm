; Write a procedure `an-integer-between` that returns an integer between to
; given bounds. This can be used to implement a procedure that find Pythagorean
; triples, i.e., triples of integers *(i, j, k)* between the given bounds such
; that *i <= j*, and *i^2 + j^2 = k^2*, as follows:
;
; ...

(load "ch4-ambeval.scm")

(let* ((env (setup-environment))
       (local-eval (lambda (exp) (eval exp env))))

  (driver-loop)

  (define (require p)
    (if (not p) (amb)))

  (define (an-integer-starting-from n)
    (amb n (an-integer-starting-from (+ n 1))))

  (define (an-integer-between n m)
    (cond ((= n m) n)
          ((> n m) (amb))
          (else (amb n (an-integer-between (+ n 1) m)))))

  (an-integer-between 10 15)

  try-again

  (let ((i (an-integer-between 10 20)))
    (let ((j (an-integer-between 10 20)))
      (require (= (+ i j)
                  30))
      (list i j)))

  (define (a-pythagorean-triple-between low high)
    (let ((i (an-integer-between low high)))
      (let ((j (an-integer-between i high)))
        (let ((k (an-integer-between j high)))
          (require (= (+ (* i i)
                         (* j j))
                      (* k k)))
          (list i j k)))))

  (a-pythagorean-triple-between 6 20)

  try-again

  (define (a-pythagorean-triple-from low)
    (let ((i (an-integer-starting-from low)))
      (let ((j (an-integer-starting-from i)))
        (let ((k (an-integer-starting-from j)))
          (require (= (+ (* i i)
                         (* j j))
                      (* k k)))
          (list i j k)))))

  (a-pythagorean-triple-from 1)

  try-again

  )

(display (amb))

(driver-loop)

(amb 2 3 4)

