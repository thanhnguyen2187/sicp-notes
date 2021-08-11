; Exercise 2.23
;
; The procedure `for-each` is similar to `map`. It takes as arguments as a
; procedure and a list of elements. However, rather than forming a list of the
; results, `for-each` just applies the procedure to each of the elements in
; turn, from left to right. The values returned by applying the procedure to the
; elements are not used at all -- `for-each` is used with procedures that
; perform an action, such as printing. For example,
;
; ...
;
; The value returned to the call to `for-each` (not illustrated) above can be
; something arbitrary, such as true. Give an implementation of `for-each`.

#lang sicp

(define (foreach xs
                 f)
  (if (null? (cdr xs))
      (f (car xs))
      (and (f (car xs))
           (foreach (cdr xs)
                    f))))

(foreach (list 1 2 3 4 5)
         (lambda (x) (and (display x)
                          (newline))))
