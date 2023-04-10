; Alyssa P. Hacker wants a *breakpoint* feature in the simulator to help her
; debug her machine designs. You have been hired to install this feature for
; her. She wants to be able to specify a place in the controller sequence where
; the simulator will stop and allow her to examine the state of the machine. You
; are implement a procedure
;
;   (set-breakpoint <machine> <label> <n>)
;
; that sets a breakpoint just before the nth instruction after the given label.
; For example,
;
;   (set-breakpoint gcd-machine 'test-b 4)
;
; installs a breakpoint in `gcd-machine` just before the assignment to register
; `a`. When the simulator reaches the breakpoint, it should print the label and
; the offset of the breakpoint and stop executing instructions. Alyssa can then
; use `get-register-contents` and `set-register-contents!` to manipulate the
; state of the simulated machine. She should then be able to continue execution
; by saying
;
;   (proceed-machine <machine>)
;
; She should also be able to remove a specific breakpoint by means of
;
;   (cancel-breakpoint <machine> <label> <n>)
;
; or to remove all breakpoints by means of
;
;   (cancel-all-breakpoints <machine>)
