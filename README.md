
<p align="center">
    <img width="200px" src="docs/src/assets/logo.svg"/>
</p>

<div align="center">

[![](https://img.shields.io/badge/Documentation-dev-blue.svg)](https://jakobjpeters.github.io/PAQ.jl/dev/) [![codecov](https://codecov.io/gh/jakobjpeters/PAQ.jl/branch/main/graph/badge.svg?token=XFWU66WSD7)](https://codecov.io/gh/jakobjpeters/PAQ.jl)

</div>


## Introduction

If you lke logic, then you've come to the right place!

P∧Q has an intuitive interface that enables you to manipulate logical statements symbolically. This is thanks to Julia's support for [Unicode](https://docs.julialang.org/en/v1/manual/unicode-input/), infix operators, and inductive data structures.

The implementation concise, only ~200 source lines of code (according to Codecov as of December, 2022).


## Getting Started

```jldoctest
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
