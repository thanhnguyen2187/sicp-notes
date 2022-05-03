; Ben Bitdiddle claims that the following method for generating Pythagorean
; triples is more efficient than the one in Exercise 4.35. Is he correct? (Hint:
; Consider the number of possibilities that must be explored.)
;
; ---
;
; Ben is right, since the "search tree" within this version is probably a bit
; smaller than the version within 4.35.

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

(a-pythagorean-triple-between 1 100)

try-again
