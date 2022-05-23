; Write an ordinary Scheme program to solve the multiple dwelling puzzle.

(define (filter predicate sequence)
  (if (null? sequence)
    '()
    (if (not (predicate (car sequence)))
      (filter predicate
              (cdr sequence))
      (cons (car sequence)
            (filter predicate (cdr sequence))))))

(define (remove elements to-be-removed)
  (if (null? elements)
    elements
    (let ((first-element (car elements))
          (rest-elements (cdr elements)))
      (if (equal? first-element to-be-removed)
        rest-elements
        (cons first-element
              (remove rest-elements to-be-removed))))))

(define (generate-permutations elements)
  (if (null? elements)
    '(())
    (apply append
           (map (lambda (first-element)
                  (map (lambda (new-permutation)
                         (cons first-element new-permutation))
                       (generate-permutations (remove elements first-element))))
                elements))))

(define (solve)

  (define floors '(1 2 3 4 5))
  (define dwellers '(baker cooper fletcher miller smith))
  (define (valid-solution? solution)
    (let ((baker    (car    solution))
          (cooper   (cadr   solution))
          (fletcher (caddr  solution))
          (miller   (cadddr solution))
          (smith    (car    (cddddr solution))))
      (and (not (= baker 5))
           (not (= cooper 1))
           (not (= fletcher 1))
           (not (= fletcher 5))
           (> miller cooper)
           (not (= (abs (- smith fletcher))
                   1))
           (not (= (abs (- fletcher cooper))
                   1))
           )))

  (define solutions (generate-permutations floors))
  (define valid-solutions
    (filter valid-solution?
            solutions))

  valid-solutions)

(solve)

