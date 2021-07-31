# Chapter 2: Building Abstractions with Data

> [...in chapter 1], we saw how to:
> - use primitive data (numbers) and primitive operations (arithmetic operations)
> - combine procedures to form compound procedures through
>   - compositions,
>   - conditionals,
>   - and the use of parameters
>   - how to abstract procedures by using `define`
> - a procedure can be regarded as a pattern for the local evolution of a
>   process
> - higher-order procedures enhance the power of our language by enabling us to
>   manipulates, and thereby to reason in terms of, general methods of
>   computation

> Programs are typically designed to model complex phenomena, and more often
> than not one must construct computational objects that have several parts
> in order to model real-world phenomena that have several aspects.

> [...] key aspect[s] of any programming language:
> - building abstractions by combining procedures to form compound procedures
> - building abstractions by combining data objects to form compound data

> Why do we want compound data in a programming language? [...]
> - elevate the conceptual level at which we can design our programs,
> - increase the modularity of our design, and to
> - enhance the expressive power of our language

> [...] isolating the parts of a program that deal with:
> - how data objects are represented [...]
> - how data objects are used
> is a powerful design methodology called **data abstraction**.

> [...] the key to forming compound data is that a programming language should
> provide some kind of "glue" so that data objects can be to form more complex
> data objects.

> One key idea in dealing with compound data is the notion of /closure/ -- that:
> the glue we used for combining data objects should allow us to
> - combine not only primitive data objects,
> - but compound data objects as well.

> [...] augment the representational power of our language by introducing
> /symbolic expressions/ -- data whose elementary parts can be arbitrary symbols
> rather than only numbers.

> [...] just as a given numerical function can be computed by many different
> computational processes, there are many ways which a given data structure can
> be represented in terms of simpler objects, and the choice of representation
> can have significant impact on the time and space requirements of processes
> that manipulate the data.

> [...] working with data that may be represented differently by different parts
> of a program. This leads to the need to implement /generic operations/, which
> must handle many different types of data.

> Maintaining modularity in the presence of generic operations require more
> powerful abstraction barriers than can be erected with simple data abstraction
> alone. [...], we introduce /data-directed programming/ as a technique that
> allows individual data representations to be designed in isolation and then
> combined /additively/ (i.e. without modification).
