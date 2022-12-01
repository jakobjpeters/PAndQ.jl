
"""
    Language

Set of well-formed logical formulae.

Supertype of [`Primitive`](@ref), [`Compound`](@ref), and [`Truth`](@ref).
"""
abstract type Language end

"""
    Compound <: Language

Compound proposition.

Subtype of [`Language`](@ref).
Supertype of [`Propositional`](@ref) and [`Modal`](@ref).
"""
abstract type Compound <: Language end

"""
    Primitive <: Language
    Primitive([statement::String = ""])

Primitive proposition.

Subtype of [`Language`](@ref).
See also [`Compound`](@ref).

# Examples
```jldoctest
julia> p = Primitive("Logic is fun")
Primitive("Logic is fun")

julia> ¬p
Propositional(
  Not(), Propositional(
    Primitive("Logic is fun")
  )
)
```
"""
struct Primitive <: Language
    statement::String

    Primitive(statement::String = "") = new(statement)
end

"""
    Operator

Set of functions that operate on [`well-formed formulae`](@ref Language).

Supertype of [`Boolean`](@ref) and [`Modal`](@ref).
"""
abstract type Operator end

"""
    Boolean <: Operator

Set of functionally complete logical connectives.

Subtype of [`Operator`](@ref).
Supertype of [`Not`](@ref) and [`And`](@ref).
See also [Boolean Operators](@ref).
"""
abstract type Boolean <: Operator end

"""
    Not <: Boolean <: Operator

Singleton type representing logical negation that operates on a [`well-formed formulae`](@ref Language).

Subtype of [`Boolean`](@ref) and [`Operator`](@ref).
See also [`And`](@ref).

# Examples
```jldoctest; setup = :(@primitive p)
julia> @truth_table PAQ.Not()(p)
┌───────────┬────────────────┐
│         p │ (PAQ.Not())(p) │
│ Primitive │  Propositional │
├───────────┼────────────────┤
│         ⊤ │              ⊥ │
│         ⊥ │              ⊤ │
└───────────┴────────────────┘
```
"""
struct Not <: Boolean end
const _not = Not()

"""
    And <: Boolean <: Operator

Singleton type representing logical conjunction that operates on two [`well-formed formulae`](@ref Language).

Subtype of [`Boolean`](@ref) and [`Operator`](@ref).
See also [`Not`](@ref).

# Examples
```jldoctest; setup = :(@primitive p q)
julia> @truth_table PAQ.And()(p, q)
┌───────────┬───────────┬───────────────────┐
│         p │         q │ (PAQ.And())(p, q) │
│ Primitive │ Primitive │     Propositional │
├───────────┼───────────┼───────────────────┤
│         ⊤ │         ⊤ │                 ⊤ │
│         ⊤ │         ⊥ │                 ⊥ │
├───────────┼───────────┼───────────────────┤
│         ⊥ │         ⊤ │                 ⊥ │
│         ⊥ │         ⊥ │                 ⊥ │
└───────────┴───────────┴───────────────────┘
```
"""
struct And <: Boolean end
const _and = And()

"""
    Propositional{
        L <: Union{
            Primitive,
            Tuple{Not, Compound},
            Tuple{And, Compound, Compound}
        }
    } <: Compound <: Language
    Propositional(ϕ::L)

Abstract syntax tree representing a compound proposition.

Subtype of [`Compound`](@ref) and [`Language`](@ref).

See also [`Primitive`](@ref), [`Not`](@ref), and [`And`](@ref).

# Examples
```jldoctest
julia> p = Propositional(Primitive("p"))
Propositional(
  Primitive("p")
)

julia> ¬p
Propositional(
  Not(), Propositional(
    Primitive("p")
  )
)
```
"""
struct Propositional{
    L <: Union{
        Primitive,
        Tuple{Not, Compound},
        Tuple{And, Compound, Compound}
    }
} <: Compound
    ϕ::L
end
#=
Source:
Van Ditmarsch, Hans, et al. Handbook of epistemic logic. College Publications, 2015.
=#

Propositional(::Not, p::Compound) = Propositional((_not, p))
Propositional(::Not, p::Primitive) = Propositional(_not, Propositional(p))
Propositional(::And, p::Compound, q::Compound) = Propositional((_and, p, q))
Propositional(::And, p::Primitive, q::Language) = Propositional(_and, q, Propositional(p))
Propositional(::And, p::Compound, q::Primitive) = Propositional(_and, q, p)

"""
    Truth{V <: Union{Val{:⊥}, Val{:⊤}}} <: Language
    Truth(::V)

Container for [`Tautology`](@ref) and [`Contradiction`](@ref).
Subtype of [`Language`](@ref).
"""
struct Truth{V <: Union{Val{:⊥}, Val{:⊤}}} <: Language end

"""
    ⊥
    Contradiction

A constant which is false in every possible interpretation.

One of two valid instances of [`Truth`](@ref), the other instance being [`Tautology`](@ref).

'⊥' can be typed by '\\bot<tab>'.

# Examples
```jldoctest
julia> ¬⊥
⊤

julia> Contradiction()
⊥
```
"""
const Contradiction = Truth{Val{:⊥}}()
const ⊥ = Contradiction

"""
    ⊤
    Tautology

A constant which is true in every possible interpretation.

One of two valid instances of [`Truth`](@ref), the other instance being [`Contradiction`](@ref).

'⊤' can be typed by '\\top<tab>'.

# Examples
```jldoctest
julia> ¬⊤
⊥

julia> Tautology()
⊤
```
"""
const Tautology = Truth{Val{:⊤}}()
const ⊤ = Tautology
