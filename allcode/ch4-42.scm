; Solve the following "Liars" puzzle (from Phillips 1934):
;
; Five schoolgirls sat for an examination. Their parents -- so they thought --
; showed an undue degree of interest in the result. They therefore agreed that, in
; writing home about the examination, each girl should make one true statement and
; one untrue one. The following are the relevant passages from their letters:
;
; - Betty: "Kitty was second in the examination. I was only third"
; - Ethel: "You'll be glad to hear that I was on top. Joan was 2nd."
; - Joan: "I was third, and poor old Ethel was bottom."
; - Kitty: "I came out second. Mary was only fourth."
; - Mary: "I was fourth. Top place was taken by Betty."
;
; What in place was the order in which the five girls were placed?

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
        ((= (car items) item)
         (remove (cdr items) item))
        (else
          (cons (car items)
                (remove (cdr items) item)))))

(define (xor p1 p2)
  (or (and (not p1) p2)
      (and p1 (not p2))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (liars)
  (define remaining-positions (list 1 2 3 4 5))
  (define pick-one-position an-element-of)
  (define remove-position remove)
  (let ((betty (pick-one-position remaining-positions)))
    (define remaining-positions (remove-position remaining-positions betty))
    (let ((ethel (pick-one-position remaining-positions)))
      (define remaining-positions (remove-position remaining-positions ethel))
      (let ((joan (pick-one-position remaining-positions)))
        (define remaining-positions (remove-position remaining-positions joan))
        (let ((kitty (pick-one-position remaining-positions)))
          (define remaining-positions (remove-position remaining-positions kitty))
          (let ((mary (pick-one-position remaining-positions)))
            (require (xor (= kitty 2)
                          (= betty 3)))
            (require (xor (= ethel 1)
                          (= joan  2)))
            (require (xor (= joan  3)
                          (= ethel 5)))
            (require (xor (= kitty 2)
                          (= mary  4)))
            (require (xor (= mary  4)
                          (= betty 1)))
            (list (list 'betty betty)
                  (list 'ethel ethel)
                  (list 'joan joan)
                  (list 'kitty kitty)
                  (list 'mary mary))
            ))))))

(liars)
; ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))

try-again

