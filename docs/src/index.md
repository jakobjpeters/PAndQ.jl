
# Home

## Introduction

If you like propositional logic, then you've come to the right place!

P∧Q has an intuitive interface that enables you to manipulate logical expressions symbolically. Propositions have multiple representations which can be easily converted and extended. Several utilities have been provided for convenience, visualization, and solving propositions.

## Showcase

```julia
julia> import Pkg

julia> Pkg.add(url = "https://github.com/jakobjpeters/PAQ.jl")

julia> using PAQ

julia> ¬⊥
tautology (generic function with 1 method)

julia> @atoms p q
2-element Vector{Atom{Symbol}}:
 p
 q

julia> r = ¬p
Literal:
 ¬p

julia> t = @p (q ∧ r) → s
Tree:
 (q ∧ ¬p) → s

julia> interpret(t, p => ⊥)
Tree:
 q → s

julia> @p Clause(and, r, u, ¬v)
Clause:
 ¬p ∧ u ∧ ¬v

julia> @truth_table p ∧ ¬p r p ⊻ q (p ∨ q) ∧ (p ⊼ q)
┌────────┬──────┬──────┬─────────┬──────────────────────────┐
│ p ∧ ¬p │ p    │ q    │ ¬p      │ p ⊻ q, (p ∨ q) ∧ (p ⊼ q) │
│ Tree   │ Atom │ Atom │ Literal │ Tree, Tree               │
├────────┼──────┼──────┼─────────┼──────────────────────────┤
│ ⊥      │ ⊤    │ ⊤    │ ⊥       │ ⊥                        │
│ ⊥      │ ⊥    │ ⊤    │ ⊤       │ ⊤                        │
├────────┼──────┼──────┼─────────┼──────────────────────────┤
│ ⊥      │ ⊤    │ ⊥    │ ⊥       │ ⊤                        │
│ ⊥      │ ⊥    │ ⊥    │ ⊤       │ ⊥                        │
└────────┴──────┴──────┴─────────┴──────────────────────────┘
```

## Related Packages

- [Julog.jl](https://github.com/ztangent/Julog.jl)
- [LogicCircuits.jl](https://github.com/Juice-jl/LogicCircuits.jl)
- [Symbolics.jl](https://github.com/JuliaSymbolics/Symbolics.jl)
- [Rewrite.jl](https://github.com/HarrisonGrodin/Rewrite.jl)
- [Simplify.jl](https://github.com/HarrisonGrodin/Simplify.jl)
- [Metatheory.jl](https://github.com/JuliaSymbolics/Metatheory.jl)
- [TruthTables.jl](https://github.com/eliascarv/TruthTables.jl)
