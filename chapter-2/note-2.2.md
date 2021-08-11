# Hierarchical Data and the Closure Property

> The ability to create pairs whose elements are pairs is the essence of list
> structure's importance as a representational tool. We refer to this ability as
> the /closure property/ of `cons`.

> Closure is the key to power in any means of combination because it permits us
> to create /hierarchical/ structures -- structures made up of parts, which
> themselves are made up of parts, and so on.

> The use of the word "closure" here comes from abstract algebra, where:
> - a set of elements is said to be closed under an operation,
> - if applying the operation to elements in the set produces an element that is
>   again, an element of the set.
   
> The difference between the two definitions [using `map`, and not using `map`]
> is not that the computer is performing a different process (is isn't), but
> that we think about the process differently. In effect, `map` helps establish
> an abstraction barrier that isolates the implementation of procedures that
> transform lists from the details of how the elements of the list are extracted
> an combined. [...] this astraction gives us the flexibility to change the
> low-lever details of how sequences are implemented, while preserving the
> conceptual framework of operations that transform sequences to sequences.

> The value of expressing programs as sequence of operations is that this helps
> us make program designs that are modular, that is, designs that are
> constructed by combining relatively independent pieces.

## 2.2.4 Example: A Picture Language

> [...] the approach of /stratified design/, the notion that a complex system
> should be structured as a sequence of levels that are described using a
> sequence of language.
