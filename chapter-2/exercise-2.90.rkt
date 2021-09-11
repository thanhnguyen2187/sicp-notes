; Exercise 2.90
;
; Suppose we want to have a polynomial system that is efficient for both sparse
; and dense polynomials. One way to do this is to allow both kinds of
; `term-list` representation in our system. This situation is analogous to the
; complex-number example of Section 2.4, where we allowed both rectangular and
; polar representations. To do this we must distinguish different types of term
; lists generic.
;
; Redesign the polynomial system to implement this generalization. This a major
; effor, not a local change.

#lang sicp

(define (put) 0)
(define (get) 0)

(define (attach-tag type-tag contents)
  (cons (type-tag contents)))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG")))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS")))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC " (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-dense-package)

  ;; internal procedures

  (define (make-poly variable term-list)
    (cons variable
          term-list))
  (define (variable  p) (car p))
  (define (term-list p) (cdr p))

  (define (=zero?) 0)

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (first-term      term-list) (car   term-list))
  (define (rest-terms      term-list) (cdr   term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car  term))
  (define (coeff term) (cadr term))

  (define (add-terms L1 L2) 0)
  (define (sub-terms L1 L2) 0)
  (define (mul-terms L1 L2) 0)

  (define (add-poly p1 p2) 0)
  (define (sub-poly p1 p2) 0)
  (define (mul-poly p1 p2) 0)

  ;; interface to the rest of the system

  (define (tag p) (attach-tag 'dense p))

  (put 'make-poly 'dense
       (lambda (variable term-list) (tag (make-poly variable term-list))))

  (put 'add-poly  '(dense dense) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub-poly  '(dense dense) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul-poly  '(dense dense) (lambda (p1 p2) (tag (mul-poly p1 p2))))

  'done)

(define (install-sparse-package)

  ;; internal procedures

  (define (make-poly variable coeff-list)
    (cons variable
          coeff-list))
  (define (variable   p) (car p))
  (define (coeff-list p) (cdr p))

  (define (zip-left L1
                    L2
                    default-element)
    ;; Pad `default-element`s to the left side of the shorter list,
    ;; and then return the two.
    ;;
    ;;   > (pad (list 1 2 3) (list 4) 0)
    ;;   ((list 1 2 3) (list 0 0 4))
    (cond ((= (length L1)
              (length L2)) (list L1 L2))
          ((< (length L1)
              (length L2))
           (zip-left (cons default-element L1)
                     L2))
          ((> (length L1)
              (length L2))
           (zip-left L2
                     (cons default-element L1)))))
  (define (repeat n element)
    (if (= n 1)
        (list element)
        (cons element
              (repeat (- n 1) element))))
  (define (pad-right n element L)
    ;; Pad `n` `element`s to the right side of list `L`
    ;;
    ;;  > (pad-right 3 0 (list 3 4 5))
    ;;  '(3 4 5 0 0 0)
    (append L (repeat n element)))

  (define (add-coeffs L1 L2)
    ;; Since `L1` and `L2` both are lists, a `zip-left`-ed `map` is enough
    (let ((Ls (zip-left L1 L2 0)))
      (map + (car Ls)
             (cdr Ls))))

  (define (sub-coeffs L1 L2)
    (let ((Ls (zip-left L1 L2 0)))
      (map - (car Ls)
             (cdr Ls))))

  (define (mul-coeffs L1 L2)
    ;; Take the first element of L1, multiply it with L2 to get new
    ;; coefficients, then repeat the same process with the second element of L1.
    ;; Finally, condense the the results into one.
    (if (null? L1)
        L2
        (let ((L1-first-mul-L2 (map (lambda (x) (* x (car L1)))
                                    ; replace `*` with `mul` to make the
                                    ; multiplication works with any kind of
                                    ; coefficient number
                                    L2)))
          (let ((L1-first-mul-L2-padded
                  ; need the padding to correct the order of the coefficients
                  (pad-right (length L1)
                             0
                             L1-first-mul-L2)))
            (add-coeffs L1-first-mul-L2-padded
                        (mul-coeffs (cdr L1)
                                    L2))))))

  (define (add-poly p1 p2)
    (add-coeffs (coeff-list p1)
                (coeff-list p2)))
  (define (sub-poly p1 p2)
    (sub-coeffs (coeff-list p1)
                (coeff-list p2)))
  (define (mul-poly p1 p2)
    (mul-coeffs (coeff-list p1)
                (coeff-list p2)))

  ;; interface to the rest of the system

  (define (tag p) (attach-tag 'sparse p))

  (put 'make-poly 'sparse
       (lambda (variable term-list) (tag (make-poly variable term-list))))

  (put 'add-poly  '(sparse sparse) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub-poly  '(sparse sparse) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul-poly  '(sparse sparse) (lambda (p1 p2) (tag (mul-poly p1 p2))))

  'done)

