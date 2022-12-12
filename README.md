
# P∧Q.jl

Do you like logic? If so, then you've come to the right package! Check out the [documentation](https://jakobjpeters.github.io/PAQ.jl/).


## Introduction

P∧Q.jl is designed to have an intuitive interface by enabling you to write code in logical symbols. This is thanks to Julia's support for [Unicode](https://docs.julialang.org/en/v1/manual/unicode-input/) and infix operators. Alternatively, every symbol has a written alias.

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

julia> ((⊥ ⊼ r) → ¬(q ⊻ ⊤)) ∨ (⊥ ∧ p)
Primitive("q")
```

## Getting Started

```julia
julia> import Pkg

julia> Pkg.add(url = "https://github.com/jakobjpeters/PAQ.jl")

julia> using PAQ
```


## Known Bugs
