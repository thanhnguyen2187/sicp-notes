# Metalinguistic Abstraction

... expert programmers control the complexity of their designs with the same
general techniques used by designers of all complex systems. They

- combine primitive elements to form compound objects,
- ... abstract compound objects to form higher-level building blocks,
- ... preserve modularity by adopting appropriate large-scale views of system
  structure

... Establishing new languages is a powerful strategy for controlling complexity
in engineering design; we can often enhance our ability to deal with a complex
problem by adopting a new language that enables us to describe (and hence to 
think about) the problem in a different way, using

- primitives,
- means of combination, and 
- means of abstractions

that are particularly well suited to the problem at hand.

Programming is endowed with a multitude of languages. There are

- Physical languages, such as the machine languages for particular computers.
  These languages are concerned with the representation of data and control in
  terms of individual bits of storage and primitive machine instructions. The
  machine-language programmer is concerned with using the given hardware to
  erect systems and utilities for the efficient implementation of
  resource-limited computations.
- High-level languages, erected on a machine-language substrate, hide concerns
  about the representation of data as a collections of bits and the
  representation of programs as sequence of primitive instructions. These
  languages have means of combination and abstraction, such as procedure
  definition, that are appropriate to the larger-scale organization of systems.

*Metalinguistic abstraction* -- establishing new languages -- plays an important
role in all branches of engineering design. It is particularly important to
computer programming, because in programming not only can we formulate new
languages but we can also implement these languages by constructing evaluators.

An *evaluator* (or *interpreter*) for a programming language is a procedure
that,

- when applied to an expression of the language,
- performs the actions required to evaluate that expression.

It is no exaggeration to regard this as the most fundamental idea in
programming:

```
The evaluator, which determines the meaning of expression in a programming
language, is just another program.
```

...

Seen from this perspective, the technology for coping with large-scale computer
systems merges with the technology for building new computer languages, and
computer science itself becomes no more (and no less) than the discipline of
constructing appropriate descriptive languages.

