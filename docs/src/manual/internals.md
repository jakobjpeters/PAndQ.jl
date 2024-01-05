
# Internals

Everything that is not `export`ed is considered internal.

```@docs
PAndQ.union_typeof
```

## PicoSAT

```@autodocs
Modules = [PicoSAT]
```

## [Operators](@id internals_operators)

```@docs
PAndQ.FoldDirection
PAndQ.Left
PAndQ.Right
PAndQ.InitialValue
PAndQ.HasInitialValue
PAndQ.NoInitialValue
PAndQ.initial_value
```

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

```@docs
PAndQ.value_exception
```

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
PAndQ.negated_normal
PAndQ.distribute
PAndQ.flatten!
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
PAndQ.neutral_operator
PAndQ.eval_doubles
PAndQ.combine
PAndQ.negated_normal_template
convert(::Type{PAndQ.Atom}, ::PAndQ.Literal{typeof(𝒾)})
```
