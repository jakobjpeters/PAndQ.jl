
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

### Types

```@example
import AbstractTrees: children # hide
using AbstractTrees: print_tree # hide
using InteractiveUtils: subtypes # hide
using PAndQ: Proposition # hide

children(x::Type) = subtypes(x) # hide
print_tree(Proposition) # hide
```

#### Abstract

```@docs
PAndQ.Proposition
PAndQ.Atom
PAndQ.Compound
PAndQ.Expressive
```

#### Concrete

```@docs
PAndQ.Constant
PAndQ.Variable
PAndQ.Literal
PAndQ.Tree
PAndQ.Clause
PAndQ.Normal
```

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
PAndQ.parenthesize
PAndQ.print_node
PAndQ.show_atom
```

## Semantics

```@docs
PAndQ.neutral_operator
PAndQ.eval_doubles
```
