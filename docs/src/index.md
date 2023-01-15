
```@meta
DocTestSetup = quote
    using PAQ
end
```

# Home

## Introduction

If you like propositional logic, then you've come to the right place!

P∧Q has an intuitive interface that enables you to manipulate logical statements symbolically. The implementation is concise, with only ~200 source lines of code (according to Codecov as of December, 2022).


## Installation

```julia
julia> import Pkg

julia> Pkg.add(url = "https://github.com/jakobjpeters/PAQ.jl")

julia> using PAQ
```


## Showcase

```
julia> ¬⊥
Truth:
  ⊤

julia> @atom p q

julia> r = p → q
Tree:
  ¬("p" ∧ ¬"q")

julia> Valuation(r)
Valuation:
  ["p" => ⊤, "q" => ⊤] => ⊤
  ["p" => ⊤, "q" => ⊥] => ⊥
  ["p" => ⊥, "q" => ⊤] => ⊤
  ["p" => ⊥, "q" => ⊥] => ⊤

julia> Normal(or, r)
Normal:
  ("p" ∧ "q") ∨ (¬"p" ∧ "q") ∨ (¬"p" ∧ ¬"q")

julia> s = @pretty p ↔ q
Pretty{Tree}:
  p ↔ q

julia> s.p
Tree:
  ¬("p" ∧ ¬"q") ∧ ¬(¬"p" ∧ "q")

julia> @truth_table r s ¬(p ⊻ q) ⊥
┌──────┬──────┬──────┬─────────────┬───────┐
│ p    │ q    │ r    │ s, ¬(p ⊻ q) │ ⊥     │
│ Atom │ Atom │ Tree │ Tree        │ Truth │
│ "p"  │ "q"  │      │             │       │
├──────┼──────┼──────┼─────────────┼───────┤
│ ⊤    │ ⊤    │ ⊤    │ ⊤           │ ⊥     │
│ ⊤    │ ⊥    │ ⊥    │ ⊥           │ ⊥     │
├──────┼──────┼──────┼─────────────┼───────┤
│ ⊥    │ ⊤    │ ⊤    │ ⊥           │ ⊥     │
│ ⊥    │ ⊥    │ ⊤    │ ⊤           │ ⊥     │
└──────┴──────┴──────┴─────────────┴───────┘
```

## Related Packages
- [Julog.jl](https://github.com/ztangent/Julog.jl)
- [LogicCircuits.jl](https://github.com/Juice-jl/LogicCircuits.jl)
- [Symbolics.jl](https://github.com/JuliaSymbolics/Symbolics.jl)
- [Rewrite.jl](https://github.com/HarrisonGrodin/Rewrite.jl)
- [Metatheory.jl](https://github.com/JuliaSymbolics/Metatheory.jl)
