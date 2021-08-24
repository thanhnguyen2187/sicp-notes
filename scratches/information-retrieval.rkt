; Exercise ?.?

#lang sicp

(define (key) 0)
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records)
         false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key
                      (cdr set-of-records)))))
