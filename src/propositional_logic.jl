
import Base: convert

# Types

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
Supertype of [`Literal`](@ref), [`Propositional`](@ref), and [`Normal`](@ref).
"""
abstract type Compound <: Language end

"""
    Operator

Set of functions that operate on a logical [`Language`](@ref).

Supertype of [`Boolean`](@ref).
"""
abstract type Operator end

"""
    Boolean <: Operator

Set of logical connectives.

Subtype of [`Operator`](@ref).
Supertype of [`Not`](@ref), [`And`](@ref), and [`Or`](@ref).
See also [Boolean Operators](@ref).
"""
abstract type Boolean <: Operator end

"""
    Not <: Boolean <: Operator

Singleton type representing logical negation.

Subtype of [`Boolean`](@ref) and [`Operator`](@ref).
See also [`not`](@ref).
```
"""
struct Not <: Boolean end

"""
    And <: Boolean <: Operator

Singleton type representing logical conjunction.

Subtype of [`Boolean`](@ref) and [`Operator`](@ref).
See also [`and`](@ref).
```
"""
struct And <: Boolean end

"""
    Or <: Boolean <: Operator

Singleton type representing logical disjunction.

Subtype of [`Boolean`](@ref) and [`Operator`](@ref).
See also [`or`](@ref).
"""
struct Or <: Boolean end

"""
    Primitive <: Language
    Primitive([statement::String = ""])

Primitive proposition.

Subtype of [`Language`](@ref).

# Examples
```jldoctest
julia> p
Primitive:
  p

julia> p()
Contingency:
  [p => ⊤] => ⊤
  [p => ⊥] => ⊥
```
"""
struct Primitive <: Language
    statement::String

    Primitive(statement::String = "") = statement == "" ? new("_") : new(statement)
end

"""
    Literal{
        L <: Union{
            Primitive,
            Tuple{Not, Primitive}
        }
    } <: Compound <: Language
    Literal(p::L)

A [`Primitive`](@ref) or its negation.

Subtype of [`Compound`](@ref) and [`Language`](@ref).
See also [`Not`](@ref).

# Examples
```jldoctest
julia> r = ¬p
Literal:
  ¬p

julia> r()
Contingency:
  [p => ⊤] => ⊥
  [p => ⊥] => ⊤
```
"""
struct Literal{
    L <: Union{
        Primitive,
        Tuple{Not, Primitive}
    }
} <: Compound
    ϕ::L
end

"""
    Propositional{
        L <: Union{
            Tuple{Not, Compound},
            Tuple{And, Compound, Compound}
        }
    } <: Compound <: Language
    Propositional(ϕ::L)

Abstract syntax tree representing a compound proposition.

Subtype of [`Compound`](@ref) and [`Language`](@ref).
See also [`Not`](@ref) and [`And`](@ref).

# Examples
```jldoctest
julia> r = p ∧ ¬p
Propositional:
  p ∧ ¬p

julia> r()
Truth:
  ⊥

julia> (p ∧ q)()
Contingency:
  [p => ⊤, q => ⊤] => ⊤
  [p => ⊤, q => ⊥] => ⊥
  [p => ⊥, q => ⊤] => ⊥
  [p => ⊥, q => ⊥] => ⊥
```
"""
struct Propositional{
    L <: Union{
        Tuple{Not, Compound},
        Tuple{And, Compound, Compound}
    }
} <: Compound
    ϕ::L
end

"""
    Normal{B <: Union{And, Or}} <: Compound <: Language
    Normal(::B, clauses::Vector{Vector{Literal}})

The conjunctive or disjunctive normal form of a proposition.

Constructing an instance with the parameters ```(And(), p)``` and ```(Or(), p)```
correspond to conjunctive and disjunctive normal form, respectively.

Subtype of [`Compound`](@ref) and [`Language`](@ref).

# Examples
```jldoctest
julia> r = Normal(∧, p ∧ q)
Normal:
  (¬p ∨ q) ∧ (p ∨ ¬q) ∧ (p ∨ q)

julia> s = Normal(∨, ¬p ∨ ¬q)
Normal:
  (p ∧ ¬q) ∨ (¬p ∧ q) ∨ (¬p ∧ ¬q)

julia> t = r ∧ s
Propositional:
  (¬p ∨ q) ∧ (p ∨ ¬q) ∧ (p ∨ q) ∧ (p ∧ ¬q) ∨ (¬p ∧ q) ∨ (¬p ∧ ¬q)

julia> t()
Truth:
  ⊥
```
"""
struct Normal{B <: Union{And, Or} #=, L <: Literal =#} <: Compound
    clauses::Vector{Vector{Literal}}
end

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
Truth:
  ⊥

julia> tautology()
Truth:
  ⊤
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
Truth:
  ⊤

julia> contradiction()
Truth:
  ⊥
```
"""
const contradiction = Truth{Val{:⊥}}()
const ⊥ = contradiction

"""
    Contingency

# Examples
```jldoctest
julia> p()
Contingency:
  [p => ⊤] => ⊤
  [p => ⊥] => ⊥

julia> (p ∧ q)()
Contingency:
  [p => ⊤, q => ⊤] => ⊤
  [p => ⊤, q => ⊥] => ⊥
  [p => ⊥, q => ⊤] => ⊥
  [p => ⊥, q => ⊥] => ⊥
```
"""
struct Contingency <: Language
    interpretations::Vector{Pair{Vector{Pair{Primitive, Truth}}}}
end


# Utility

"""
    @primitive(ps...)

Instantiates [`Primitive`](@ref) propositions.

Examples
```jldoctest
julia> @primitive p q

julia> p
Primitive:
  p

julia> q
Primitive:
  q
```
"""
macro primitive(expressions...)
    primitive = expression -> :($(esc(expression)) = Primitive($(string(expression))))
    primitives = map(primitive, expressions)

    return :($(primitives...); nothing)
end
#=
Source:
https://github.com/ctrekker/Deductive.jl
=#

"""
    get_primitives(ps::Language...)

Returns a vector of [`Primitive`](@ref) propositions contained in ```ps```.

Note that some primitives may optimized out of an expression, such as in ```p ∧ ⊥```.

See also [`Language`](@ref).

# Examples
```jldoctest
julia> r = p ∧ q
Propositional:
  p ∧ q

julia> get_primitives(r)
2-element Vector{Primitive}:
 p
 q
```
"""
get_primitives(::Truth) = []
get_primitives(p::Contingency) = union(
    mapreduce(
        interpretation -> map(
            literal -> first(literal), first(interpretation)),
        vcat, p.interpretations
    )
)
get_primitives(p::Primitive) = [p]
get_primitives(p::Literal) = get_primitives(p.ϕ)
get_primitives(p::Propositional) = union(get_primitives(p.ϕ))
get_primitives(ϕ::Tuple{Operator, Vararg}) = mapreduce(p -> get_primitives(p), vcat, Base.tail(ϕ))
get_primitives(p::Normal) = get_primitives(Propositional(p))
get_primitives(ps::Language...) = union(mapreduce(get_primitives, vcat, ps))


# Helpers 

Base.convert(::Type{L}, p::L) where L <: Language = p
Base.convert(::Type{L}, p::Language) where L <: Language = L(p)
Base.convert(::Type{Literal}, literal::Pair{Primitive, <:Truth}) = last(literal) == ⊤ ? Literal(first(literal)) : ¬first(literal)
Base.convert(::Type{Propositional}, p::typeof(⊥)) = Primitive() ∧ ¬Primitive()
Base.convert(::Type{Propositional}, p::typeof(⊤)) = ¬Propositional(⊥)
Base.convert(::Type{Propositional}, p::Contingency) = mapreduce(interpretation -> mapreduce(Literal, ∧, first(interpretation)), ∨, filter(interpretation -> last(interpretation) == ⊤, p.interpretations))
Base.convert(n::Type{Normal}, p::Contingency) = n(Propositional(p))
Base.convert(::Type{Propositional}, p::Normal{And}) = _convert(p, ∨, ∧)
Base.convert(::Type{Propositional}, p::Normal{Or}) = _convert(p, ∧, ∨)
_convert(p, inner, outer) = mapreduce(clause -> reduce(inner, clause), outer, p.clauses)


# Consructors

# TODO: use Base.promote?
Propositional(::Not, p::Primitive) = Literal((Not(), p))
Propositional(::Not, p::Compound) = Propositional((Not(), p))

Propositional(::And, p::Primitive, q::Primitive) = Propositional(And(), Literal(p), Literal(q))
Propositional(::And, p::Primitive, q::Compound) = Propositional(And(), Literal(p), q)
Propositional(::And, p::Compound, q::Primitive) = Propositional(And(), p, Literal(q))
Propositional(::And, p::Compound, q::Compound) = Propositional((And(), p, q))

Literal(p::Pair{Primitive, Truth}) = convert(Literal, p)
Propositional(p::Language) = convert(Propositional, p)
Normal(::B, p::Contingency) where B <: Union{And, Or} = convert(Contingency{B}, p)

Normal(::And, p::Language) = ¬Normal(Or(), ¬p)
function Normal(::Or, p::Language)
    q = p()
    interpretations = () ->
        if q === ⊤
            [[Primitive() => ⊤], [Primitive() => ⊥]]
        elseif q === ⊥
            [[Primitive() => ⊤, Primitive() => ⊥]]
        else
            map(first, filter(literal -> last(literal) == ⊤, q.interpretations))
        end

    clauses = map(interpretation -> map(Literal, interpretation), interpretations())
    return Normal{Or}(clauses)
end
