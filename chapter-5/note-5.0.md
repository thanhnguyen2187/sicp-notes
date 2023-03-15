# 5. Computing with Register Machines

> In this chapter we will describe processes in terms of the step-by-step
> operation of a traditional computer. Such a computer, or register machine,
> sequentially executes instructions that manipulate the contents of a fixed set
> of storage elements called registers.

> A typical register-machine instruction:
>
> - Applies a primitive operation to the contents of some registers and
> - Assigns the results to another register

> In Section 5.2 we will implement a Lisp program that uses these descriptions
> to simulate the machines we design.

> In Section 5.3 we will study their implementation (car, cdr, and cons) in
> terms of more elementary operations.

> In Section 5.4, after we have accumulated experience formulating simple
> procedures as register machines, we will design a machine that carries out the
> algorithm described by the metacircular evaluator of Section 4.1.

> In Section 5.5 we will study a simple compiler that translates Scheme programs
> into sequences of instructions that can be executed directly with the
> registers and operations of the evaluator register machine.

## 5.1. Designing Register Machines

> To design a register machine, we must design its:
>
> - *data paths* (registers and operations), and
> - the *controller* that sequences these operations.

```lisp
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
```

> A machine to carry out this algorithm must keep track of two numbers, *a* and
> *b*, so let us assume that these numbers are stored in two registers with
> those names.
>
> The basic operations required are:
>
> - Testing whether the contents of register `b` is zero and
> - Computing the remainder of the contents of register `a` divided by the
>   contents of register `b`.
>
> The remainder is a complex process, but assume for the moment that we have a
> primitive device that computes remainders.
>
> On each cycle of the `GCD` algorithm,
>
> - The contents of register `a` must be replaced by the contents of register
>   `b`, and
> - The contents of `b` must be replaced by the remainder of the old contents of
>   `a` divided by the old contents of `b`.
>
> It would be convenient if these replacements could be done simultaneously, but
> in our model of register machines we will assume that only one register can be
> assigned to a new value at each step.
>
> To accomplish the replacements, our machine will use a third "temporary"
> register, which we call `t`.
>
> - First, the remainder will be placed in `t`, then
> - The contents of `b` will be placed in `a`, and finally
> - The remainder stored in `t` will be placed in `b`.
