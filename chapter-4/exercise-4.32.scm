;; For streams

((lambda ()

   (define (delay x)
     (lambda () x))

   (define (force x)
     (x))

   (define (cons-stream x y)
     (cons x (delay y)))

   (define (stream-car s) (car s))
   (define (stream-cdr s) (force (cdr s)))
   (define the-empty-stream '())

   (define ones
     (cons-stream 1 the-empty-stream))
   (set-cdr! ones (delay ones))

   (define (stream-ref s n)
     (if (= n 0)
       (stream-car s)
       (stream-ref (stream-cdr s) (- n 1))))

   (display (stream-ref ones 0))
   (display (stream-ref ones 1))
   (display (stream-ref ones 2))
   (display (stream-ref ones 99))

   'done))

;; For lazy evaluation

((lambda ()
   ; (define (cons x y) (lambda (m) (m x y)))
   ; (define (car z) (z (lambda (p q) p)))
   ; (define (cdr z) (z (lambda (p q) q)))

   (define ones (cons 1 ones))))
