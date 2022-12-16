
# Types

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
```jldoctest
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
```jldoctest
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
    }(ϕ::L) <: Compound <: Language

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
    Truth{V <: Union{Val{:⊥}, Val{:⊤}}}(::V) <: Language

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

# Function-like objects

function (p::Language)()
    primitives = get_primitives(p)
    n = length(primitives)
    truth_sets = multiset_permutations([⊤, ⊥], [n, n], n)
    valuations = map(truth_set -> map(Pair{Primitive, Truth}, primitives, truth_set), truth_sets)
    truths = map(valuation -> interpret(p -> Dict(valuation)[p], p), valuations)
    return map(Pair, valuations, truths)
end

(::Not)(p::Language) = Propositional(_not, p)
(::Not)(p::Propositional{Tuple{Not, Propositional{P}}}) where P <: Primitive = last(p.ϕ).ϕ # double negation elimination
(::Not)(p::Propositional{Tuple{Not, C}}) where C <: Compound = last(p.ϕ) # double negation elimination
(::Not)(p::Truth{Val{:⊤}}) = ⊥
(::Not)(p::Truth{Val{:⊥}}) = ⊤

(::And)(p::Language, q::Language) = Propositional(_and, p, q)
(::And)(::Truth{Val{:⊤}}, ::Truth{Val{:⊤}}) = ⊤
(::And)(::Truth{Val{:⊤}}, q::Language) = q # identity law
(::And)(::Truth{Val{:⊥}}, q::Language) = ⊥ # domination law
(::And)(p::Union{Primitive, Compound}, q::Truth) = q ∧ p # commutative law

# Utility

"""
    @primitive(ps...)

Instantiates [`Primitive`](@ref) propositions.

Examples
```jldoctest
julia> @primitive p q

julia> p
Primitive("p")

julia> q
Primitive("q")
```
"""
macro primitive(expressions...)
    primitive = expression -> :($(esc(expression)) = Primitive($(string(expression))))
    primitives = map(primitive, expressions)

    return quote
        $(primitives...)
        nothing
    end
end
#=
Source:
https://github.com/ctrekker/Deductive.jl
=#

"""
    get_primitives(ps::Language...)

Returns a vector of [`Primitive`](@ref) propositions contained in ```p```.

Note that some primitives may optimized out of an expression, such as in ```p ∧ ⊥```.

See also [`Language`](@ref).

# Examples
```jldoctest
julia> get_primitives(p)
1-element Vector{Primitive}:
 Primitive("p")

julia> get_primitives(p ∧ q, r)
3-element Vector{Primitive}:
 Primitive("p")
 Primitive("q")
 Primitive("r")
```
"""
get_primitives(ps::Language...) = union(mapreduce(get_primitives, vcat, ps))
get_primitives(p::Compound) = union(get_primitives(p.ϕ))
get_primitives(ϕ::Tuple{Operator, Vararg}) = mapreduce(p -> get_primitives(p.ϕ), vcat, Base.tail(ϕ))
get_primitives(p::Primitive) = [p]
get_primitives(::Truth) = Primitive[]
