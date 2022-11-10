
"""
    Language

Set of well-formed logical formulae.

Supertype of [`Primitive`](@ref), [`Compound`](@ref), and [`Valuation`](@ref).
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
    Primitive{S <: Union{String, Nothing}} <: Language
    Primitive{S}([s = nothing])

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
struct Primitive{S <: Union{String, Nothing}} <: Language
    statement::S

    Primitive(s::S = nothing) where S = new{S}(s)
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
```jldoctest
julia> truth_table(PAQ.Not())
2-element Vector{Pair}:
 ⊤ => ⊥
 ⊥ => ⊤
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
```jldoctest
julia> truth_table(PAQ.And())
4-element Vector{Pair}:
 (⊤, ⊥) => ⊥
 (⊥, ⊥) => ⊥
 (⊤, ⊤) => ⊤
 (⊥, ⊤) => ⊥
```
"""
struct And <: Boolean end
const _and = And()

"""
    Propositional{
        L <: Union{
            Primitive,
            Tuple{Not, Language},
            Tuple{And, Language, Language}
        }
    } <: Compound <: Language
    Propositional{L}(ϕ)

Abstract syntax tree representing a compound proposition.

Subtype of [`Compound`](@ref) and [`Language`](@ref).

See also [`Primitive`](@ref), [`Not`](@ref), and [`And`](@ref).

# Examples
```jldoctest
julia> p = Propositional(Primitive())
Propositional(
  Primitive(nothing)
)

julia> ¬p
Propositional(
  Not(), Propositional(
    Primitive(nothing)
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

"""
    Valuation{V <: Union{Val{:⊥}, Val{:⊤}}} <: Language
    Valuation(::V)

Container for [`Tautology`](@ref) and [`Contradiction`](@ref).
Subtype of [`Language`](@ref).
"""
struct Valuation{V <: Union{Val{:⊥}, Val{:⊤}}} <: Language end

"""
    ⊥
    Contradiction

A constant which is false in every possible interpretation.

One of two valid instances of [`Valuation`](@ref), the other instance being [`Tautology`](@ref).

'⊥' can be typed by '\\bot<tab>'.

# Examples
```jldoctest
julia> ¬⊥
⊤

julia> Contradiction()
Propositional(
  And(), Propositional(
    Primitive(nothing)
  ) Propositional(
    Not(), Propositional(
      Primitive(nothing)
    ) 
  ) 
) 
```
"""
const Contradiction = Valuation{Val{:⊥}}()
const ⊥ = Contradiction

"""
    ⊤
    Tautology

A constant which is true in every possible interpretation.

One of two valid instances of [`Valuation`](@ref), the other instance being [`Contradiction`](@ref).

'⊤' can be typed by '\\top<tab>'.

# Examples
```jldoctest
julia> ¬⊤
⊥

julia> Tautology()
Propositional(
  Not(), Propositional(
    And(), Propositional(
      Primitive(nothing)
    ) Propositional(
      Not(), Propositional(
        Primitive(nothing)
      ) 
    ) 
  ) 
)
```
"""
const Tautology = Valuation{Val{:⊤}}()
const ⊤ = Tautology
