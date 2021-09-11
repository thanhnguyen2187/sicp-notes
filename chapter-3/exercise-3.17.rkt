; Exercise 3.17
;
; Devise a correct version of the `count-pairs` procedure in Exercise 3.16 that
; returns the number of distinct pairs in any structure.
;
; (Hint: Traverse the structure, maintaining an auxilary data structure that is
; used to keep track of which pairs have already been counted.)

#lang sicp

(define (count-pairs x)

  (define (within encountered-pairs x)
    (if (null? encountered-pairs)
        false
        (or (eq? (car encountered-pairs) x)
            (within (cdr encountered-pairs) x))))

  (define (recurse encountered-pairs
                   x)
    ; (display x) (newline)
    ; (display encountered-pairs) (newline)
    ; (if (or (within encountered-pairs x)
    ;         (not (pair? x)))
    ;     0
    ;     1)
    (if (or (within encountered-pairs x)
            (not (pair? x)))
        0
        (let ((newly-encountered-pairs (cons x encountered-pairs)))
          (+ (recurse newly-encountered-pairs (car x))
             (recurse newly-encountered-pairs (cdr x))
             1)))
    )

  (recurse '() x))

; (count-pairs (list 1 2 3))
; (count-pairs (list 1 2 3 4))
; (count-pairs (list 1 2 3 4 5 6 7))
; 
(define x (list '1 '2))
(set-cdr! x x)

(count-pairs x)

; (count-pairs (list 1 2))
; (count-pairs 1)
