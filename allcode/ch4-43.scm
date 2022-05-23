; Use the `amb` evaluator to solve the following puzzle.
;
; Mary Ann Moore's father has a yacht and so has each of his four friends:
; Colonel Downing, Mr. Hall, Sir Barnacle Hood, and Dr. Parker. Each of the five
; also has one daughter and each has named his yacht after a daughter of one of
; the others. Sir Barnacle's yacht is the Gabrielle, Mr. Moore owns the Lorna;
; Mr. Hall the Rosalind. The Melissa, owned by Colonel Downing, is named after
; Sir Barnacle's daughter. Gabrielle's father owns the yacht that is named after
; Dr. Parker's daughter. Who is Lorna's father?
;
; Try to write the program so that it runs efficiently (see Exercise 4.40). Also
; determine how many solutions there are if we are not told that Mary Ann's last
; name is Moore.
;
; | Father   | Yatch     | Daughter  |
; | ---      | ---       | ---       |
; | Barnacle | Gabrielle | Meilssa   |
; | Moore    | Lorna     | Mary      |
; | Hall     | Rosalind  | ?         |
; | Downing  | Meilssa   | ?         |
; | Parker   | Mary      | ?         |

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
        (list 'equal? equal?)
        (list 'cadr cadr)
        (list 'caddr caddr)
        (list '< <)
        (list '<= <=)
        (list 'and (lambda (clause-1 clause-2) (and clause-1 clause-2)))
        (list 'or (lambda (clause-1 clause-2) (or clause-1 clause-2)))
        (list 'map map)
        (list 'length length)
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

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define (a-permutation-of items)
  (n-elements-of (length items) items))

(define (zip sequence-1 sequence-2)
  (if (or (null? sequence-1)
          (null? sequence-2))
    '()
    (cons (list (car sequence-1)
                (car sequence-2))
          (zip (cdr sequence-1)
               (cdr sequence-2)))))

; (define (map procedure sequence)
;   (if (null? sequence)
;     '()
;     (cons (procedure (car sequence))
;           (map procedure (cdr sequence)))))

; (define (filter predicate sequence)
;   (if (null? sequence)
;     '()
;     (let ((first-element (car sequence))
;           (rest-elements (filter predicate (cdr sequence))))
;       (if (predicate first-element)
;         (cons first-element
;               rest-elements)
;         rest-elements))))

(define (first predicate sequence)
  (if (null? sequence)
    '()
    (if (predicate (car sequence))
      (car sequence)
      (first predicate (cdr sequence)))))

(define (solve)
  (define fathers   (list 'barnacle  'moore 'hall     'downing 'parker))
  (define yachts    (list 'gabrielle 'lorna 'rosalind 'melissa 'mary))
  ; (define daughters (list 'gabrielle 'lorna 'rosalind 'melissa 'mary))

  (define (yacht-of owner)
    (cond ((eq? owner 'barnacle) 'gabrielle)
          ((eq? owner 'moore)    'lorna)
          ((eq? owner 'hall)     'rosalind)
          ((eq? owner 'downing)  'melissa)
          ((eq? owner 'parker)   'mary)
          ))

  (define (owner-of yacht)
    (cond ((eq? yacht 'gabrielle) 'barnacle)
          ((eq? yacht 'lorna)     'moore)
          ((eq? yacht 'rosalind)  'hall)
          ((eq? yacht 'melissa)   'downing)
          ((eq? yacht 'mary)      'parker)
          ))

  (let ((daughters (a-permutation-of yachts)))

    (define father-daughter-pairs
      (zip fathers daughters))

    (define (father-of daughter)
      (car (first (lambda (pair) (eq? daughter (cadr pair)))
                  father-daughter-pairs)))
    (define (daughter-of father)
      (cadr (first (lambda (pair) (eq? father (car pair)))
                   father-daughter-pairs)))

    (require (not (eq? (father-of 'gabrielle)
                       (owner-of  'gabrielle))))
    (require (not (eq? (father-of 'rosalind)
                       (owner-of  'rosalind))))
    (require (not (eq? (father-of 'melissa)
                       (owner-of  'melissa))))
    (require (not (eq? (father-of 'lorna)
                       (owner-of  'lorna))))
    (require (not (eq? (father-of 'mary)
                       (owner-of  'mary))))

    (require (eq? (father-of 'melissa)
                  'barnacle))
    ; (require (eq? (father-of 'mary)
    ;               'moore))
    (require (eq? (yacht-of (father-of 'gabrielle))
                  (daughter-of 'parker)))

    (list 'fathers   fathers
          'yachts    yachts
          'daughters daughters)
    ))

(solve)

try-again

; The solution is:
;
; | Father   | Yatch     | Daughter  |
; | ---      | ---       | ---       |
; | Barnacle | Gabrielle | Meilssa   |
; | Moore    | Lorna     | Mary      |
; | Hall     | Rosalind  | Gabrielle |
; | Downing  | Meilssa   | Lorna     |
; | Parker   | Mary      | Rosalind  |
;
; Without the requirement that Mary's father is Moore, there is going to be
; another solution, where Mary's father is Hall, and Gabrielle's father is
; Moore.
;
; | Father   | Yatch     | Daughter  |
; | ---      | ---       | ---       |
; | Barnacle | Gabrielle | Meilssa   |
; | Moore    | Lorna     | Gabrielle |
; | Hall     | Rosalind  | Mary      |
; | Downing  | Meilssa   | Lorna     |
; | Parker   | Mary      | Rosalind  |

