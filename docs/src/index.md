
```@meta
DocTestSetup = :(using PAndQ)
```

# Home

## Introduction

If you like propositional logic, then you've come to the right place!

P∧Q has an intuitive interface that enables you to manipulate logical expressions symbolically. Propositions have multiple representations which can be easily converted and extended. Several utilities have been provided for convenience, visualization, and solving propositions.

## Installation

```julia
julia> using Pkg: add

julia> add("PAndQ")

julia> using PAndQ
```

## Showcase

```jldoctest
julia> ¬⊥
tautology (generic function with 1 method)

julia> @variables p q
2-element Vector{Variable}:
 p
 q

julia> r = ¬p
¬p

julia> s = Clause(and, [p, ¬q])
p ∧ ¬q

julia> @atomize t = ((q ∧ r) ↔ a)(a => ⊤)
(q ∧ ¬p) ↔ ⊤

julia> u = s ∨ t
(p ∧ ¬q) ∨ (q ∧ ¬p)

julia> TruthTable([p ∧ ¬p, r, p ⊻ q, u])
┌────────┬───┬───┬────┬────────────────────────────┐
│ p ∧ ¬p │ p │ q │ ¬p │ p ⊻ q, (p ∧ ¬q) ∨ (q ∧ ¬p) │
├────────┼───┼───┼────┼────────────────────────────┤
│ ⊥      │ ⊤ │ ⊤ │ ⊥  │ ⊥                          │
│ ⊥      │ ⊥ │ ⊤ │ ⊤  │ ⊤                          │
├────────┼───┼───┼────┼────────────────────────────┤
│ ⊥      │ ⊤ │ ⊥ │ ⊥  │ ⊤                          │
│ ⊥      │ ⊥ │ ⊥ │ ⊤  │ ⊥                          │
└────────┴───┴───┴────┴────────────────────────────┘
```

## Related Projects

`PAndQ.jl` is currently best suited for learning and visualizing propositional logic. The user interface is well-documented and has numerous quality of life features that make it easy to create, manipulate, and query about propositions. Further, propositions can be converted, visualized, and output in several different forms.

Features such as a fast satisfiability algorithm, proofs, and other logics are still in development. In the meantime, check out the these excellent projects.

### Top Picks

- [Julog.jl](https://github.com/ztangent/Julog.jl)
- [LogicCircuits.jl](https://github.com/Juice-jl/LogicCircuits.jl)
- [SoleLogics.jl](https://github.com/aclai-lab/SoleLogics.jl)
- [FirstOrderLogic.jl](https://github.com/roberthoenig/FirstOrderLogic.jl)
- [ModalDecisionTrees.jl](https://github.com/aclai-lab/ModalDecisionTrees.jl)

### Computer Algebra Systems

- [Symbolics.jl](https://github.com/JuliaSymbolics/Symbolics.jl)
- [Metatheory.jl](https://github.com/JuliaSymbolics/Metatheory.jl)
- [Rewrite.jl](https://github.com/HarrisonGrodin/Rewrite.jl)
- [Simplify.jl](https://github.com/HarrisonGrodin/Simplify.jl)
- [SymbolicUtils.jl](https://github.com/JuliaSymbolics/SymbolicUtils.jl)

### Constraint Solvers

- [ConstraintSolver.jl](https://github.com/Wikunia/ConstraintSolver.jl)
- [JuMP.jl](https://github.com/jump-dev/JuMP.jl)

#### Wrappers

- [Chuffed.jl](https://github.com/JuliaConstraints/Chuffed.jl)
- [CPLEXCP.jl](https://github.com/JuliaConstraints/CPLEXCP.jl)
- [JaCoP.jl](https://github.com/JuliaConstraints/JaCoP.jl)

### Wrappers

- [Satifsiability.jl](https://github.com/elsoroka/Satisfiability.jl)
- [Z3.jl](https://github.com/ahumenberger/Z3.jl)
- [PicoSat.jl](https://github.com/sisl/PicoSAT.jl)
- [BeeEncoder.jl](https://github.com/newptcai/BeeEncoder.jl)
- [SatisfiabilityInterface.jl](https://github.com/dpsanders/SatisfiabilityInterface.jl)

### Honorable Mentions

- [TruthTables.jl](https://github.com/eliascarv/TruthTables.jl)
- [SimpleSATSolver.jl](https://github.com/dpsanders/SimpleSATSolver.jl)
- [PropositionalLogic.jl](https://github.com/mossr/PropositionalLogic.jl)
- [MathematicalPredicates.jl](https://github.com/JuliaReach/MathematicalPredicates.jl)
- [combinators](https://git.devin.gay/devin/combinators)

#### Kanren

- [MuKanren.jl](https://github.com/latticetower/MuKanren.jl)
- [LilKanren.jl](https://github.com/habemus-papadum/LilKanren.jl)
- [MiniKanren](https://github.com/RAbraham/MiniKanren)
- [Yet Another MicroKanren in Julia](https://www.philipzucker.com/yet-another-microkanren-in-julia/)
