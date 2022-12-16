
"""
    Language

Set of well-formed logical formulae.

Calling an instance of ```Language``` will return a vector of valid interpretations.

Supertype of [`Primitive`](@ref), [`Compound`](@ref), and [`Truth`](@ref).
```
"""
abstract type Language end

"""
    Compound <: Language

Compound proposition.

Subtype of [`Language`](@ref).
Supertype of [`Propositional`](@ref).
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
julia> p
Primitive("p")

julia> p()
2-element Vector{Pair{Vector{Pair{Primitive, Truth}}}}:
 [Primitive("p") => ⊤] => ⊤
 [Primitive("p") => ⊥] => ⊥
```
"""
struct Primitive <: Language
    statement::String

    Primitive(statement::String = "") = new(statement)
end

"""
    Operator

Set of functions that operate on a logical [`Language`](@ref).

Supertype of [`Boolean`](@ref).
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

Singleton type representing logical negation.

Subtype of [`Boolean`](@ref) and [`Operator`](@ref).
See also [`And`](@ref) and [`Language`](@ref).

# Examples
```jldoctest; setup = :(@primitive p)
julia> @truth_table PAQ.Not()(p)
┌───────────┬────────────────┐
│ p         │ (PAQ.Not())(p) │
│ Primitive │ Propositional  │
│ "p"       │                │
├───────────┼────────────────┤
│ ⊤         │ ⊥              │
│ ⊥         │ ⊤              │
└───────────┴────────────────┘
```
"""
struct Not <: Boolean end
const _not = Not()

"""
    And <: Boolean <: Operator

Singleton type representing logical conjunction.

Subtype of [`Boolean`](@ref) and [`Operator`](@ref).
See also [`Not`](@ref) and [`Language`](@ref).

# Examples
```jldoctest; setup = :(@primitive p q)
julia> @truth_table PAQ.And()(p, q)
┌───────────┬───────────┬───────────────────┐
│ p         │ q         │ (PAQ.And())(p, q) │
│ Primitive │ Primitive │ Propositional     │
│ "p"       │ "q"       │                   │
├───────────┼───────────┼───────────────────┤
│ ⊤         │ ⊤         │ ⊤                 │
│ ⊤         │ ⊥         │ ⊥                 │
├───────────┼───────────┼───────────────────┤
│ ⊥         │ ⊤         │ ⊥                 │
│ ⊥         │ ⊥         │ ⊥                 │
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
julia> p ∧ ¬p
Propositional(
  And(), Propositional(
    Primitive("p")
  ) Propositional(
    Not(), Propositional(
      Primitive("p")
    ) 
  ) 
)

julia> (p ∧ ¬p)()
2-element Vector{Pair{Vector{Pair{Primitive, Truth}}, Truth{Val{:⊥}}}}:
 [Primitive("p") => ⊤] => ⊥
 [Primitive("p") => ⊥] => ⊥

julia> (p → q) ∧ (p ← q) == ¬(p ⊻ q)
true
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
# preserve ordering
Propositional(::And, p::Primitive, q::Compound) = Propositional(_and, Propositional(p), q)
Propositional(::And, p::Compound, q::Primitive) = Propositional(_and, p, Propositional(q))
Propositional(::And, p::Primitive, q::Primitive) = Propositional(_and, Propositional(p), Propositional(q))
Propositional(::And, p::Compound, q::Compound) = Propositional((_and, p, q))
Propositional(::Not, p::Compound) = Propositional((_not, p))
Propositional(::Not, p::Primitive) = Propositional(_not, Propositional(p))

"""
    Truth{V <: Union{Val{:⊥}, Val{:⊤}}} <: Language
    Truth(::V)

Container for the constants [`tautology`](@ref) and [`contradiction`](@ref).
Subtype of [`Language`](@ref).
"""
struct Truth{V <: Union{Val{:⊥}, Val{:⊤}}} <: Language end

"""
    ⊤
    tautology

A constant which is true in every possible interpretation.

One of two valid instances of [`Truth`](@ref), the other instance being [`contradiction`](@ref).

```⊤``` can be typed by ```\\top<tab>```.

# Examples
```jldoctest
julia> ¬⊤
⊥

julia> tautology()
1-element Vector{Pair{Vector{Pair{Primitive, Truth}}, Truth{Val{:⊤}}}}:
 [] => ⊤
```
"""
const tautology = Truth{Val{:⊤}}()
const ⊤ = tautology

"""
    ⊥
    contradiction

A constant which is false in every possible interpretation.

One of two valid instances of [`Truth`](@ref), the other instance being [`tautology`](@ref).

```⊥``` can be typed by ```\\bot<tab>```.

# Examples
```jldoctest
julia> ¬⊥
⊤

julia> contradiction()
1-element Vector{Pair{Vector{Pair{Primitive, Truth}}, Truth{Val{:⊥}}}}:
 [] => ⊥
```
"""
const contradiction = Truth{Val{:⊥}}()
const ⊥ = contradiction
