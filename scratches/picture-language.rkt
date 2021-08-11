; Exercise ?.?

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
(define up-split 0)
(define rotate180 0)
(define make-frame 0)

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))
(define (flipped-pairs painter)
  (let ((painter2 (beside painter
                          (flip-vert painter))))
    (below painter2
           painter2)))

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

(define (square-of-four top-left
                        top-right
                        bottom-left
                        bottom-right)
  (lambda (painter)
    (let ((top    (beside (top-left    painter) (top-right    painter)))
          (bottom (beside (bottom-left painter) (bottom-right painter))))
      (below bottom top))))

(define (flipped-pairs-v2 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (add-vect) #t)
(define (sub-vect) #t)
(define (origin-frame) #t)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame))))

(define (transform-painter painter
                           origin
                           corner1
                           corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                             (sub-vect (m corner1)
                                       new-origin)
                             (sub-vect (m corner2)
                                       new-origin)))))))
