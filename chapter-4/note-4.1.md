## 4.1 The Metacircular Evaluator

... An evaluator that is written in the same language that it evaluates is said
to be *metacircular*.

The metacircular evaluator is essentially a Scheme formulation of the
environment model of evaluation described in Section 3.2. Recall that the model
has two basic parts:

1. To evaluate a combination (a compound expression other than a special form),
   - evaluate the subexpressions and then
   - apply the value of the operator subexpression to the values of the operand
     subexpressions.
2. To apply a coumpound procedure to a set of arguments, evaluate the body of
   the procedure in a new environment. To construct this environment, extend the
   environment part of the procedure object by a frame in which the formal
   parameters of the procedure are bound to the arguments to which the procedure is
   applied.

### 4.1.1 The core of the Evaluator

The evaluation process can be described as the interplay between two procedures:
`eval` and `apply`.

#### Eval

`eval` takes as arguments an expression and an environment. It classifies the
expression and directs its evaluation.
