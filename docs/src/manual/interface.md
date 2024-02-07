
# Interface

These functions define the interface for logical operators. The behavior of the built-in functions is specified using this interface, so custom operators can be implemented by implementing each of the relevant methods. Default implementations are not provided so as to avoid correctness bugs.

## Required Methods

```@docs
Interface.Operator
Interface.Evaluation
Interface.Eager
Interface.Lazy
Interface.FoldDirection
Interface.Left
Interface.Right
Interface.initial_value
Interface.evaluate
Interface.arity
Interface.symbol_of
Interface.pretty_print
Interface.dual
```

## Optional Methods

These methods are not currently required for the currently implemplemented features, but may be useful.

```@docs
Interface.converse
Interface.is_associative
Interface.is_commutative
```
