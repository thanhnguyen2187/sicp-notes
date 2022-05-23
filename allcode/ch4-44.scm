; Exercise 2.42 described the "eight-queens puzzle" of placing queens on a
; chessboard so that no two attack each other. Write a nondeterministic program
; to sovle this puzzle.

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
        (list 'cadr cadr)
        (list 'caddr caddr)
        (list '< <)
        (list '<= <=)
        (list 'and (lambda (clause-1 clause-2) (and clause-1 clause-2)))
        (list 'or (lambda (clause-1 clause-2) (or clause-1 clause-2)))
        ))

;;(define the-global-environment (setup-environment))
(driver-loop)

(restart 1)

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items)
       (an-element-of (cdr items))))

(define (remove items item)
  (cond ((null? items)
         items)
        ((eq? (car items) item)
         (remove (cdr items) item))
        (else
          (cons (car items)
                (remove (cdr items) item)))))

(define (n-elements-of n items)
  (if (or (= n 0)
          (null? items))
    '()
    (let ((picked (an-element-of items)))
      (cons picked
            (n-elements-of (- n 1)
                           (remove items picked))))))

(define (a-permutation-of items)
  (n-elements-of (length items) items))

(define (range start stop step)
  (if (>= start stop)
    '()
    (cons start
          (range (+ start step)
                 stop
                 step))))

(define (length sequence)
  (if (null? sequence)
    0
    (+ 1 (length (cdr sequence)))))

(define (map procedure sequence)
  (if (null? sequence)
    '()
    (cons (procedure (car sequence))
          (map procedure (cdr sequence)))))

(define (reduce procedure sequence initial)
  (if (null? sequence)
    initial
    (procedure (car sequence)
               (reduce procedure (cdr sequence) initial))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (zip sequence-1 sequence-2)
  (if (or (null? sequence-1)
          (null? sequence-2))
    '()
    (cons (list (car sequence-1)
                (car sequence-2))
          (zip (cdr sequence-1)
               (cdr sequence-2)))))

(define (solve board-size)

  (define xs (a-permutation-of (range 0 board-size 1)))
  (define ys (a-permutation-of (range 0 board-size 1)))
  (define (x-of position)
    (car position))
  (define (y-of position)
    (cadr position))

  (define positions
    (zip xs ys))

  (define diagonals-1
    (map (lambda (position)
           (- (x-of position)
              (y-of position)))
         positions))

  (define diagonals-2
    (map (lambda (position)
           (+ (x-of position)
              (y-of position)))
         positions))

  (require (distinct? diagonals-1))
  (require (distinct? diagonals-2))

  positions)

(solve 5)

try-again

