# 2 Introduction to Data Abstraction

## 2.1 Abstraction Barriers

> In general, the underlying idea of data abstraction is:
> - to identify for each type of data object a basic set of operations
> - in terms of which all manipulations of data objects of that type will be
>   expressed
> - and then to use only those operations in manipulating the data

```
================================================================================

--[Programs that use rational numbers]--

Rational numbers in problem domain

--[`add-rat` `sub-rat` ...]--

Rational numbers as numerators and denominators

--[`make-rat` `numer` `denom`]--

Rational numbers as pairs

--[`cons` `car` `cdr`]--

However pairs are implemented

================================================================================
```

> The horizontal lines represent /abstraction barriers_/ that isolate different
> "levels" of the system.

> At each level, the barrier seperates:
> - the programs (above) that use the data abstraction from
> - the programs (below) that implement the data abstraction.

> This simple idea has many advantages.

> One advantage is that it makes programs much easier to maintain and modify.
> Any complex data structure can be represented in a variety of ways with the
> primitive data structures provided by a programming language.

> [...] the choice of representation influences the programs that operate on it;
> thus, if the representation were to be changed at some later time, all such
> programs might have to be modified accordingly. This task could be
> time-consuming and expensive in the case of large programs unless the
> dependence on the representation were to be confined by design to a very few
> pgoram modules.

> Constraining the dependence on the representation to a few interface
> procedures helps us design programs as well as modify them, because it allows
> us to maintain the flexibility to consider alternate implementations.

### 2.1.3 What is Meant by Data?

> In general, we can think of data as:
> - defined by some collection of selectors and constructors,
> - together with specified conditions that these procedures must fulfill in
>   order to be a valid representation

> [...] the ability to manipulate procedures as objects automatically provides
> the ability to represent compound data. [...]. This style of programming is
> often called /message passing/, [...]
