; In the multiple dwelling problem, how many sets of assignments are there of
; people to floors, both before and after the requirement that floor assignments
; be distinct? It is very inefficient to generate all possible assignments of
; people to floors and then leave it to backtracking to eliminate them. For
; example, most of the restrictions depend only on one or two of the
; person-floor variables, and can thus be imposed before floors have been
; selected for all the people. Write and demonstrate a much more efficient
; nondeterministic procedure that solve this problem based on generating only
; those possibilities that are not already ruled out by previous restrictions.
; (Hint: This will require a nest of `let` expressions.)
;
; ---
;
; > How many sets of assignments are there of people to floors, both before and
; after the requirement that floor assignments to be distinct?
;
; Before the requirement, there are 5^5 set of assignments. After the
; requirement, the set of assignments are reduced to 5!.

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

(define (multiple-dwelling)
  (define remaining-floors (list 1 2 3 4 5))
  (define pick-one-floor an-element-of)
  (define remove-from-floors remove)
  (let ((baker (pick-one-floor remaining-floors)))
    (require (not (= baker 5)))
    (define remaining-floors (remove-from-floors remaining-floors baker))
    (let ((cooper (pick-one-floor remaining-floors)))
      (require (not (= cooper 1)))
      (define remaining-floors (remove-from-floors remaining-floors cooper))
      (let ((fletcher (pick-one-floor remaining-floors)))
        (require (not (= fletcher 1)))
        (require (not (= fletcher 5)))
        (define remaining-floors (remove-from-floors remaining-floors fletcher))
        (let ((miller (pick-one-floor remaining-floors)))
          (define remaining-floors (remove-from-floors remaining-floors miller))
          (let ((smith (pick-one-floor remaining-floors)))
            (require (> miller cooper))
            (require (not (= (abs (- smith fletcher))
                             1)))
            (require (not (= (abs (- fletcher cooper))
                             1)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))

(multiple-dwelling)

try-again

