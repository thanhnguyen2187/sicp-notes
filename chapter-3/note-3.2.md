## 3.2. The Environment Model of Evaluation

[...]

* To apply a compound procedure to arguments, evaluate the body of the procedure
  with each formal parameter replaced by the corresponding argument.

[...] An environment is sequence of frames. Each frame is a table (possibly
empty) of _bindings_, which associate variable names with their corresponding
values. (A single frame may contain at most one binding for any variable.)

Each frame also has a pointer to its _enclosing environment_, unless, for the
purposes of discussion, the frame is considered to be _global_. The _value of a
variable_ with respect to an environment is the value given by the binding of
the variable in the first frame in the environment that contains a binding for
that variable. If no frame in the sequence specifies a binding for the variable,
then the variable is said to be _unbound_ in the environment.
