# 1.3 Formulating Abstractions with Higher-Order Procedures

> One of the things we should demand from a powerful programming language is the
> ability to build abstractions by:
> - assigning names to common patterns and then
> - work in terms of the abstractions directly

> Often the same programming pattern will be used with a number of different
> procedures.
> 
> To express such patterns as concepts, we will need to construct procedures
> that can accept procedures as arguments or return procedures as values.
> Procedures that manipulate procedures are called _higher-order procedures_.

## 1.3.4 Procedure as Returned Values

> In general, programming languages impose restrictions on the ways in which
> computational elements can be manipulated. Elements with the fewest
> restrictions are said to have /first-class/ status. Some of the "right and
> privileges" of the first-class elements are:
>
> - They may be named by variables.
> - They may be passed as arguments to procedures.
> - They may be returned as the results of procedure.
> - They may be included in data structures.
