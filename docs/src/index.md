
```@meta
DocTestSetup = quote
    using PAQ
end
```

# Home

## Introduction

If you lke logic, then you've come to the right place!

P∧Q has an intuitive interface that enables you to manipulate logical statements symbolically. This is thanks to Julia's support for [Unicode](https://docs.julialang.org/en/v1/manual/unicode-input/), infix operators, and inductive data structures.

The implementation concise, only ~200 source lines of code (according to Codecov as of December, 2022).


## Installation

```julia
julia> import Pkg

julia> Pkg.add(url = "https://github.com/jakobjpeters/PAQ.jl")

julia> using PAQ
```


## Showcase

```jldoctest
julia> ¬⊥
Truth:
  ⊤

julia> @primitive p q

julia> r = p ∧ q
Propositional:
  p ∧ q

julia> r()
Contingency:
  [p => ⊤, q => ⊤] => ⊤
  [p => ⊤, q => ⊥] => ⊥
  [p => ⊥, q => ⊤] => ⊥
  [p => ⊥, q => ⊥] => ⊥

julia> s = Normal(And(), r)
Normal:
  (¬p ∨ q) ∧ (p ∨ ¬q) ∧ (p ∨ q)

julia> t = @Show p ⊻ q
Show{Propositional}:
  p ⊻ q

julia> @truth_table p ⊻ q s ⊥
┌───────────┬───────────┬───────────────┬────────┬───────┐
│ p         │ q         │ p ⊻ q         │ s      │ ⊥     │
│ Primitive │ Primitive │ Propositional │ Normal │ Truth │
│ "p"       │ "q"       │               │        │       │
├───────────┼───────────┼───────────────┼────────┼───────┤
│ ⊤         │ ⊤         │ ⊥             │ ⊤      │ ⊥     │
│ ⊤         │ ⊥         │ ⊤             │ ⊥      │ ⊥     │
├───────────┼───────────┼───────────────┼────────┼───────┤
│ ⊥         │ ⊤         │ ⊤             │ ⊥      │ ⊥     │
│ ⊥         │ ⊥         │ ⊥             │ ⊥      │ ⊥     │
└───────────┴───────────┴───────────────┴────────┴───────┘
```
