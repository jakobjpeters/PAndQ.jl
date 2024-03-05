
# [Interface](@id interface)

This interface is used to implement the [built-in operators](@ref operators_operators) and can be used to implement [custom operators](@ref custom_operators).

!!! warning
    This interface is incomplete and will be changed in v0.4.

## Methods

These methods are required to be implemented for some functionalities. If a required method is not implemented, a runtime error will display the function and operator that a method must be implemented for. Default implementations of are not provided so as to avoid correctness bugs.

```@docs
Interface.Operator
Interface.arity
```

### Evaluation

```@docs
Interface.Evaluation
Interface.evaluate
```

### Folding

```@docs
Interface.Associativity
Interface.initial_value
```

### Printing

```@docs
Interface.print_expression
Interface.symbol
```

## Utilities

These functions may be necessary or useful for implementing the operator interface.

### Evaluation

```@docs
Interface.Eager
Interface.Lazy
```

### Associativity

```@docs
Interface.Left
Interface.Right
```

### Printing

```@docs
Interface.name
Interface.print_proposition
```

### Properties

```@docs
Interface.converse
Interface.dual
```

#### Predicates

```@docs
Interface.is_associative
Interface.is_commutative
```
