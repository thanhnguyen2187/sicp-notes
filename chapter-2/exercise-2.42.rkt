; Exercise 2.42
;
; The "eight-queens puzzle" asks how to place eight queens on a chessboard so
; that no quen is in check from any other (i.e., no two queens are in the same
; row, column, or diagonal). One possible solution is shown in Figure 2.8.
;
; One way to solve the puzzle is to work across the board, placing a queen in
; each column. Once we have placed /k - 1/ queens, we must place the /k^th/
; quene in a position where it does not check any of the other queens already on
; the board.
;
; We can formulate this approach recursively:
;
; - Assume that we have already generated the sequence of all possible ways to
; place /k - 1/ queens in the first /k - 1/ columns of the board.
; - For each of these ways, generate an extended set of positions by placing a
; queen in each row of the /k^th/ column.
; - Now filter these, keeping only the positions for which the quene in the
; /k^th/ column is safe with respect to the other queens.
; - This produces the sequence of all ways to place /k/ queens in the first /k/
; columns.
; - By continuing this process, we will produce not only one solution, but all
; solutions to the puzzle.
;
; We implement this solution as a procedure `queens`, which returns a sequence
; of all solutions to the problem of placing /n/ queens on an /n x n/
; chessboard.
;
; `queens` has an internal procedure `queen-cols` that returns the sequence of
; all ways to place queens in the first /k/ columns of the board.
;
; ...
;
; In this procedure `rest-of-queens` is a way to place /k - 1/ queens in the
; first /k - 1/ columns, and `new-row` is a proposed row in which to place the
; queen for the /k^th/ columns.
;
; Complete the program by implementing the representation for sets of board
; positions, including the procedure `adjoin-position`, which adjoin a new
; row-column position to a set of positions, and `empty-board`, which represents
; an empty set of positions.
;
; You must also write the procedure `safe?`, which determine for a set of
; positions, whether the queen in the /k^th/ column is safe with respect to the
; others. (Note that we need only check whether the new queen is safe -- the
; other queens are already guaranteed safe with respect to each other.)

#lang sicp

(define (enumerate-interval low
                            high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1)
                                    high))))

(define (accumulate operator
                    initial
                    sequence)
  (if (null? sequence)
      initial
      (operator (car sequence)
                (accumulate operator
                            initial
                            (cdr sequence)))))

(define (flatmap procedure
                 sequence)
  (accumulate append
              nil
              (map procedure
                   sequence)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list j i))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (filter predicate
                sequence)
  (flatmap (lambda (element)
             (if (predicate element)
                 (list element)
                 nil))
           sequence))

(define (filter-split predicate
                      sequence)
  ; use a predicate to filter the sequence, and then split the result into two
  ; sequences
  ;
  ;   (filter-split odd? (list 1 2 3 4 5 6))
  ;   -> ((1 3 5) (2 4 6))
  (if (null? sequence)
      (list (list)
            (list))
      (let ((rest (filter-split predicate (cdr sequence)))
            (append-if
              ; |                              | sequence-1 | sequence-2 |
              ; |------------------------------|------------|------------|
              ; | (predicate element) is true  | appended   | same       |
              ; | (predicate element) is false | same       | appended   |
              (lambda (sequences)
                (let ((sequence-1 (car sequences))
                      (sequence-2 (cadr sequences))
                      (element (car sequence)))
                  (if (predicate element)
                    (list (append (list element)
                                  sequence-1)
                          sequence-2)
                    (list sequence-1
                          (append (list element)
                                  sequence-2)))))))
        (append-if (filter-split predicate
                                 (cdr sequence))))))

(define (contains? sequence
                   element)
  (null? (filter (lambda (x)
                   (eq? x
                        element))
                 sequence)))

;;
(define empty-board (list))
(define (make-position row
                       column)
  (list row column))

(define (get-row position)
  (car position))
(define (get-column position)
  (cadr position))

;;
(define (same-row p1 p2)
  (= (get-row p1)
     (get-row p2)))
(define (same-column p1 p2)
  (= (get-column p1)
     (get-column p2)))
(define (same-diagonal p1 p2)
  (define ratio (/ (- (get-row p1) (get-row p2))
                   (- (get-column p1) (get-column p2))))
  (or (= ratio 1)
      (= ratio -1)))

;;
(define (safe? k
               positions)
  ; check if the queen placed in column k is conflicted with any other position
  ; by seeing if they are `same-column` or `same-row` or `same-diagonal`

  ; (display k)
  ; (newline)
  ; (display positions)
  ; (newline)
  (let ((positions-split (filter-split
                           (lambda (position)
                             (= k
                                (get-column position)))
                           positions)))
    (let ((p1   (car  (car positions-split)))
          (rest (cadr positions-split)))
      (if (null? rest)
          #t
          (contains? (map (lambda (position)
                            (or (same-column   p1 position)
                                (same-row      p1 position)
                                (same-diagonal p1 position)))
                          rest)
                     #t)))))

(define (adjoin-position new-row
                         k
                         rest-of-queens)
  (append rest-of-queens
          (list (make-position new-row k))))

(define (queens board-size)
  ; returns a sequence of all solutions to the problem of placing n queens on an
  ; n x n chessboard

  (define (queen-cols k)
    ; return the sequence of
    ; all ways to place queens in the first k columns of the board
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           ; rest-of-queens: a way to place k - 1 queens in the
                           ; first k - 1 columns,
                           (map (lambda (new-row)
                                  ; new-row: a proposed row in which to place
                                  ; the queen for the k^th columns
                                  (adjoin-position new-row
                                                   k
                                                   rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

; (define L (filter-split odd?
;                         (list 1 2 3 4 5 6 7)))
; (display L)
; (newline)
; (car L)
; (cadr L)

; (filter odd? (list 1 2 3 4 5))

(queens 8)
