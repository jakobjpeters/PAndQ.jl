
## Version 0.4.0

- Removed broken `print_dimacs` method with a file path.
- The `conjunction` and `disjunction` operators accept a sequence of propositions, rather than an iterable.
- The interface functions `print_expression` and `evaluate` are given a `Vector` of propositions, rather than a sequence.
- The interface function `arity` is no longer required to be implemented for an operator.
<!-- - Implemented canonical conjunction and disjunction normal forms. -->
- `solutions` returns a tuple where the first element is a `Vector` of atoms and the second element is an iterable of assignments. Each `Vector` of assignments is a sequence of `Bool`eans where the `i`th element corresponds to the `i`th atom.
- Implemented a Z3 backend solver.
- Implemented two-parameter `show` for `Operator`
- Stop parenthesizing sequential commutative operators
