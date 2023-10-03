
# Internals

Everything that is not `export`ed is considered internal.

```@docs
PAndQ.union_typeof
```

## [Operators](@id internals_operators)

### Union Types

```@docs
PAndQ.NullaryOperator
PAndQ.UnaryOperator
PAndQ.BinaryOperator
PAndQ.LogicalOperator
PAndQ.CommutativeOperator
PAndQ.AssociativeOperator
PAndQ.AndOr
```

## Propositions

### AbstractTrees.jl

```@docs
PAndQ.children
PAndQ.nodevalue
PAndQ.printnode
```

### Utility

```@docs
PAndQ.child
PAndQ.union_all_type
PAndQ.symbol_value
PAndQ.atomize
```

## Printing

```@docs
PAndQ.alias_of
PAndQ.symbol_of
PAndQ.merge_string
PAndQ.parenthesize
PAndQ.print_node
PAndQ.show_atom
```

## Semantics

```@docs
PAndQ.neutral_operator
PAndQ.eval_doubles
```
