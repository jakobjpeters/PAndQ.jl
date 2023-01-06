
<p align="center">
    <img width="200px" src="docs/src/assets/logo.svg"/>
</p>

<div align="center">

[![Documentation dev](https://img.shields.io/badge/Documentation-dev-blue.svg)](https://jakobjpeters.github.io/PAQ.jl/dev/)
[![Codecov](https://codecov.io/gh/jakobjpeters/PAQ.jl/branch/main/graph/badge.svg?token=XFWU66WSD7)](https://codecov.io/gh/jakobjpeters/PAQ.jl)
[![Continuous integration](https://github.com/jakobjpeters/PAQ.jl/workflows/Continuous%20Integration/badge.svg)](https://github.com/jakobjpeters/PAQ.jl/actions/continuous_integration.yml)
![License](https://img.shields.io/github/license/jakobjpeters/PAQ.jl)
<!-- ![GitHub release (latest by date)](https://img.shields.io/github/v/release/jakobjpeters/PAQ.jl) -->
<!-- [![Downloads](https://shields.io/endpoint?url=https://pkgs.genieframework.com/api/v1/badge/PAQ)](https://pkgs.genieframework.com?packages=PAQ) -->

</div>


## Introduction

If you lke propositional logic, then you've come to the right place!

P∧Q has an intuitive interface that enables you to manipulate logical statements symbolically. This is thanks to Julia's support for [Unicode](https://docs.julialang.org/en/v1/manual/unicode-input/), infix operators, and inductive data structures.

The implementation is concise, only ~200 source lines of code (according to Codecov as of December, 2022).


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

julia> t = @Pretty p ⊻ q
Pretty{Propositional}:
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
