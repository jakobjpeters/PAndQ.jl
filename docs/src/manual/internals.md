
# Internals

```@docs
PAndQ.union_typeof
```

## Solvers

### Z3

```@docs
PAndQ.Z3
PAndQ.Z3.Library
PAndQ.Z3.add_clause
PAndQ.Z3.Solutions
PAndQ.Z3.IteratorSize(::Type{PAndQ.Z3.Solutions})
PAndQ.Z3.eltype(::Type{PAndQ.Z3.Solutions})
PAndQ.Z3.isdone(::PAndQ.Z3.Solutions)
PAndQ.Z3.iterate(::PAndQ.Z3.Solutions, ::Any)
```

### PicoSAT

```@docs
PAndQ.PicoSAT
```

#### Library

```@docs
PAndQ.PicoSAT.picosat_init
PAndQ.PicoSAT.picosat_reset
PAndQ.PicoSAT.picosat_add
PAndQ.PicoSAT.picosat_adjust
PAndQ.PicoSAT.picosat_variables
PAndQ.PicoSAT.picosat_print
PAndQ.PicoSAT.picosat_sat
PAndQ.PicoSAT.picosat_deref
```

#### Utilities

```@docs
PAndQ.PicoSAT.add_clause
PAndQ.PicoSAT.initialize
PAndQ.PicoSAT.Solutions
PAndQ.PicoSAT.eltype(::Type{PAndQ.PicoSAT.Solutions})
PAndQ.PicoSAT.IteratorSize(::Type{PAndQ.PicoSAT.Solutions})
PAndQ.PicoSAT.isdone(::PAndQ.PicoSAT.Solutions)
PAndQ.PicoSAT.iterate(::PAndQ.PicoSAT.Solutions, ::Any)
PAndQ.PicoSAT.print_dimacs
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
PAndQ.AbstractSyntaxTree
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
PAndQ.deconstruct
PAndQ.child
PAndQ.load_or_error
PAndQ.atomize
PAndQ.distribute
PAndQ.prune
PAndQ.reconstruct
```

## Printing

## Semantics

```@docs
PAndQ.eval_pairs
convert
promote_rule
```
