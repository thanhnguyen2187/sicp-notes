; Exercise ?.?

#lang sicp

(define random-init 2)

(define rand (let ((x random-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

(define (estimate-pi trials)
  (sqrt (/ 6
           (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iterate trials-remaining
                   trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iterate (- trials-remaining 1)
                                 (+ trials-passed    1)))
          (else (- trials-remaining 1)
                trials-passed)))
  (iterate trials 0))

;;

(define (estimate-pi trials)
  (sqrt (/ 6
           (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0) (/ trials-passed trials))
              ((= (gcd x1 x2) 1) (iter (- trials-remaining 1)
                                       (+ trials-passed 1)
                                       x2))
              (else (iter (- trials-remaining 1)
                          trials-passed
                          x2))))))
  (iter trials 0 initial-x))
