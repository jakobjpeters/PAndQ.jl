
```@meta
DocTestSetup = quote
    using PAQ
end
```

# P∧Q.jl

Do you like logic? If so, then you've come to the right package! Check out the [source code](https://github.com/jakobjpeters/PAQ.jl/).


## Introduction

P∧Q.jl implements propositional logic ([with more to come](https://github.com/jakobjpeters/PAQ.jl/blob/main/CONTRIBUTING.md)). It is designed to have an intuitive interface by enabling you to write and evaluate logical statements symbolically. This is thanks to Julia's support for [Unicode](https://docs.julialang.org/en/v1/manual/unicode-input/) and infix operators. Alternatively, every symbol has a written alias.


## Installation

```julia
julia> import Pkg

julia> Pkg.add(url = "https://github.com/jakobjpeters/PAQ.jl")

julia> using PAQ
```


## Showcase

```jldoctest
julia> ¬⊥
⊤

julia> @primitive p q

julia> r = ¬p
Propositional(
  Not(), Propositional(
    Primitive("p")
  ) 
)

julia> ¬r
Primitive("p")

julia> p ∨ ⊤
⊤

julia> @truth_table ¬p r p → q
┌───────────┬───────────┬───────────────┬───────────────┐
│         p │         q │         ¬p, r │         p → q │
│ Primitive │ Primitive │ Propositional │ Propositional │
├───────────┼───────────┼───────────────┼───────────────┤
│         ⊤ │         ⊤ │             ⊥ │             ⊤ │
│         ⊤ │         ⊥ │             ⊥ │             ⊥ │
├───────────┼───────────┼───────────────┼───────────────┤
│         ⊥ │         ⊤ │             ⊤ │             ⊤ │
│         ⊥ │         ⊥ │             ⊤ │             ⊤ │
└───────────┴───────────┴───────────────┴───────────────┘
```
