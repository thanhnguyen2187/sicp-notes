; Exercise 2.43
;
; Louis Reasoner is having a terrible time doing Exercise 2.42. His `queens`
; produce never seems to work, but it runs extreme slowly. (Louis never does
; manage to wait long enough for it to solve even the /6 x 6/ case.)
;
; When Louis asks Eva Lu Ator for help, she points out that he has interchanged
; the order of the nested mapping in the `flatmap`, writing it as
;
; ...
;
; Explain why this interchange makes the program run slowly.
;
; Estimate how long it will take Louis's program to solve the eight-queens
; puzzle, assuming that the program in Exercise 2.42 solve the puzzle in time
; /T/.
;
; ---
;
; It will take /(n^n) * T/

#lang sicp

(define empty-board 0)
(define filter 0)
(define safe? 0)
(define flatmap 0)
(define adjoin-position 0)
(define enumerate-interval 0)

(define (queens board-size)
  ; returns a sequence of all solutions to the problem of placing n queens on an
  ; n x n chessboard

  (define (queen-cols k)
    ; return the sequence of
    ; all ways to place queens in the first k columns of the board
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (new-row)
                           ; new-row: a proposed row in which to place
                           ; the queen for the k^th columns
                           (map (lambda (rest-of-queens)
                                  ; rest-of-queens: a way to place k - 1 queens in the
                                  ; first k - 1 columns,
                                  (adjoin-position new-row
                                                   k
                                                   rest-of-queens))
                                (queen-cols (- k 1))))
                         (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

