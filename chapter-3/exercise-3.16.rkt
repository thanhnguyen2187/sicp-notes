; Exercise 3.16
;
; Ben Bitdiddle decides to write a procedure to count the number of pairs in any
; list structure.
;
; "It's easy," he reasons. "The number of pairs in any structure is the number
; of `car` plus the number in the `cdr` plus one more to count the pair." So Ben
; writes the following procedure:
;
; ...
;
; Show that his procedure is not correct.
;
; In particular, draw box-and-pointer diagrams representing list structures made
; up of extractly three pairs for which Ben's procedure would return 3; return
; 4; return 7; never return at all.
;
; ---
;
; [. .] -> [. .] -> [. .]
;  |        |        |
;  v        v        v
;  1        2        3
;
;  +-----+
;  |     |
;  v     |
; [. .]--+
;  |
;  v
;  1

#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs (list 1 2 3))
(count-pairs (list 1 2 3 4))
(count-pairs (list 1 2 3 4 5 6 7))

(define x (list '1 '2))
(set-cdr! x x)

(count-pairs x)
