## Exercise 2.77

Louis Reasoner tries to evaluate the expression `(magnitude z)` where `z` is the
object shown in Figure 2.24. To his suprise, instead of the answer `5` he gets
an error message from `apply-generic` on the types `(complex)`. He shows this
interaction to Alyssa P. Hacker, who says "The problem is that the
"complex-number" selectors were never defined `complex` numbers, just for
`polar` and `rectangular` numbers. All you have to do to make this work is add
the following to the `complex` package."

...

Define in details why this works. As an example, trace through all the
procedures called in evaluating the expression `(magnitude z)` where `z` is the
object shown in Figure 2.24. In particular, how many times is `apply-generic`
invoked? What procedure is displatched to in each case?

---

The adding works since `z` has two tags: `complex`, and `rectangular`. The first
`real-part` strips `complex` off. The second `real-part` strips `rectangular`
off. We than get the right evaluation. `apply-generic` is invoked once for each
case.
