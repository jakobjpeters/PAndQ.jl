
# Home

## Introduction

If you like propositional logic, then you've come to the right place!

P∧Q has an intuitive interface that enables you to manipulate logical statements symbolically. The implementation is concise, with only ~200 source lines of code (according to Codecov as of December, 2022).


## Showcase
```julia
julia> import Pkg

julia> Pkg.add(url = "https://github.com/jakobjpeters/PAQ.jl")

julia> using PAQ

julia> ¬⊥
Truth:
  ⊤

julia> @atoms p q
2-element Vector{Atom{Symbol}}:
 p
 q

julia> r = ¬p
Literal:
 ¬p

julia> s = @p q ∧ r
Tree:
 q ∧ ¬p

julia> interpret(s, p => ⊥)
Tree:
 q

julia> Valuation(s)
Valuation:
 [q => ⊤, p => ⊤] => ⊥
 [q => ⊥, p => ⊤] => ⊥
 [q => ⊤, p => ⊥] => ⊤
 [q => ⊥, p => ⊥] => ⊥

julia> @p v = Clause(and, r, t, ¬u)
Clause:
 ¬p ∧ t ∧ ¬u

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
