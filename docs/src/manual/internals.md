
# Internals

```@docs
PAndQ.union_typeof
```

## PicoSAT

```@autodocs
Modules = [PicoSAT]
```

## Interface

```@docs
Interface.InterfaceError
Interface.showerror
Interface.@interface
```

## [Operators](@id internals_operators)

```@docs
PAndQ.NullaryOperator
PAndQ.UnaryOperator
PAndQ.BinaryOperator
PAndQ.NaryOperator
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
PAndQ.distribute
PAndQ.flatten
```

## Printing

```@docs
PAndQ.name_of
PAndQ.minimize_io
```

## Semantics

```@docs
PAndQ.eval_doubles
convert(::Type{PAndQ.Atom}, ::PAndQ.Literal{typeof(𝒾)})
promote_rule
```
