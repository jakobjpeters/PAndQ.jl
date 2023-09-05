
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
PAndQ.LeftNeutralOperator
PAndQ.RightNeutralOperator
PAndQ.AndOr
```

## Propositions

### Union Types

```@docs
PAndQ.AtomicProposition
PAndQ.LiteralProposition
```

### AbstractTrees.jl

```@docs
PAndQ.children
PAndQ.nodevalue
PAndQ.printnode
```

### Utility

```@docs
PAndQ.union_all_type
PAndQ.only_field
PAndQ.symbol_value
PAndQ.atomize
```

## Printing

```@docs
PAndQ.operator_to_symbol
PAndQ.merge_string
PAndQ.letter
PAndQ.format_latex
PAndQ.format_head
PAndQ.format_body
PAndQ._newline
PAndQ.print_string
PAndQ.parenthesize
```

## Semantics

```@docs
PAndQ.CallableObjectDocumentation
PAndQ.neutral_operator
PAndQ.eval_doubles
```
