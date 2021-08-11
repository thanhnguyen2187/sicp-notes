; Exercise 2.44
;
; Define the procedure `up-split` used by `corner-split`. It is similar to
; `right-split`, except that it switches the roles of `below` and `beside`.

#lang sicp

(define wave 0)
(define (beside p1 p2)
  ; takes two painters and produces a new, compound painter
  ; that draws:
  ; - the first painter's image in the left half of the frame 
  ; - the second painter's image in the right half of the frame
  #t
  )
(define (below p1 p2)
  ; takes to painters and produce a compound painter that draws:
  ; - the first painter's image below the second painter's image
  #t
  )
(define (flip-vert p)
  ; takes a painter and produces a painter that draws its image upside-down
  p
  )
(define (flip-horiz p)
  ; takes a painter and produces a painter that draws its image left-to-right
  ; reversed
  p
  )
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
