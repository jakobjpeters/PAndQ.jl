
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
PAndQ.NaryOperator
PAndQ.Operator
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
```

#### Concrete

```@docs
PAndQ.Constant
PAndQ.Variable
PAndQ.Tree
PAndQ.Literal
PAndQ.Clause
PAndQ.Normal
```

### AbstractTrees.jl

```@docs
PAndQ.children
PAndQ.nodevalue
PAndQ.printnode
PAndQ.NodeType
PAndQ.nodetype
```

### Utility

```@docs
PAndQ.child
PAndQ.atomize
```

## Printing

```@docs
PAndQ.symbol_of
PAndQ.parenthesize
PAndQ.print_node
PAndQ.show_atom
PAndQ.base_type
```

## Semantics

```@docs
PAndQ.process_valuations
PAndQ.neutral_operator
PAndQ.eval_doubles
convert(::Type{PAndQ.Atom}, ::Union{PAndQ.Literal{typeof(𝒾)}, PAndQ.Tree{typeof(𝒾), <:PAndQ.Atom}})
```
