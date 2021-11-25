; Exercise 3.38
;
; Suppose that Peter, Paul, and Mary share a joint bank account that initially
; contains $100. Concurrently, Peter deposits $10, Paul withdraw $20, and Mary
; withdraws half of the money in the account, by executing the following
; commands:
;
; Paul:  `(set! balance (+ balance 10))`
; Peter: `(set! balance (- balance 20))`
; Mary:  `(set! balance (- balance (/ balance 2)))`
;
; a. List all the different possible values for `balance` after these three
; transactions have been completed, assuming that the banking system forces the
; three processes to run sequentially in some order.
; b. What are some other values that could be produced if the system allows the
; processes to be interleaved? Draw timing diagram like the one in Figure 3.29
; to explain how these values can occur.
;
; ---
;
; a. Let us set an index for those transactions: 1 for Paul's, 2 for Peter's,
; and 3 for Mary's. The different possible values are combinators of the three
; indexes:
;
; - 1 2 3: `balance = (100 + 10 - 20) / 2 = 25`
; - 1 3 2: `balance = (100 + 10) / 2 - 20 = 35`
; - 2 1 3: `balance = (100 - 20 + 10) / 2 = 25`
; - 2 3 1: `balance = (100 - 20) / 2 + 10 = 50`
; - 3 1 2: `balance = 100 / 2 + 10 - 20 = 40`
; - 3 2 1: `balance = 100 / 2 - 20 + 10 = 40`
;
; b. If the system allows the processes to be interleaved, there can be some
; "weirder" value like $80, $110.
;
; - Paul accesses balance: $100
; - Peter accesses balance: $100
; - Mary accesses balance: $100
; - Paul calculates new value: $110
; - Peter calculates new value: $80
; - Mary calculates new value: $50
; - Mary sets the new balance: $50
; - Peter sets the new balance: $80
; - Paul sets the new balance: $110

#lang sicp
