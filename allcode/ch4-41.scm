; Write an ordinary Scheme program to solve the multiple dwelling puzzle.

(define (solve)
  (define floors '(1 2 3 4 5))
  (define dwellers '(baker cooper fletcher miller smith))

  (define (recurse settled-dwellers
                   remaining-dwellers
                   remaining-floors)
    (cond ((and (null? remaining-dwellers)
                (null? floors))
           (display "Found one solution: ")
           (newline)
           (display settled-dwellers)
           (newline))
          ())))
