## Exercise 2.76

As a large system with generic operations evolves, new types of data objects or
new operations may be needed. For each of the three strategies -- generic
operations with explicit dispatch, data-directed style, and message-passing
style -- describe the changes that must be made to a system in order to add new
types or new operations.

Which organization would be most appropiate for a system in which
- New types must often be added?
- New operations must often be added?

---

### Dispatching On Type

"Generic operations with explicit dispatch" can also be called "dispatching on
type". It also is a discipline of stripping off and attaching tags as data
objects are passed from level to level.

The strategy is powerful, but also has two weakness:

- The generic interface procedures must know about all the different
  representations.
- We must guarantee that no two procedures in the entire system have the same
  name.

The issue underlying both of these weaknesses is that the technique for
implementing generic interfaces is not *additive*. The person implementing the
generic selector procedures must modify those procedures each time a new
representation is installed, and the people interfacing the individual
representations must modify their code to avoid name conflicts.

### Data-Directed Programming

Whenever we deal with a set of generic operations that are common to a set of
different types, we are dealing with a two-dimesional table that contains the
possible operations on one axis and the possible types on the other axis. The
entries in the table are the procedures that implement each operation for each
type of argument presented.

Data-directed programming is the technique of designing programs to work with
such a table directly.

### Message Passing

Message passing is an alternate strategy to data-directed programming which
decompose the table into columns, and instead of using "intelligent operations"
that dispatch on data types, to work with "intelligent data objects" that
dispatch on operation name.

The style is implemented by arranging things so that a data object is
represented as a procedure that takes as input the required operation name and
performs the operation indicated.

One limit of this organization is it permits only generic procedures of one
argument.

### Conclusion

Message-passing style is preferred for a system where new types must often be
added, since the "intelligent data objects" are not coupled, and the act of
adding new ones is additive.

Explicit-dispatching style is preferred for a system where new operations must
often be added, since (hopefully) there is not a lot of different
representations in this case.

Data-directed style works, if all else fails, but be careful with its
complexity.
