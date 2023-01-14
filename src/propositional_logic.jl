
import Base: convert

# Types

"""
Language

Set of well-formed logical formulae.

Calling an instance of ```Language``` will return a vector of valid interpretations.

Supertype of [`Atom`](@ref), [`Compound`](@ref), and [`Truth`](@ref).
```
"""
abstract type Language end

"""
Compound <: Language

Compound proposition.

Subtype of [`Language`](@ref).
Supertype of [`Literal`](@ref), [`Tree`](@ref), and [`Normal`](@ref).
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
    Atom <: Language
    Atom([statement::String])

Atomic proposition.

!!! info
  Constructing an ```Atom``` with no argument, an empty string, or an underscore character
  will set ```statement = "_"```. This serves two purposes.
  Firstly, it is useful as a default proposition when converting [`Truth`](@ref)s to other forms;
  for example: ```Tree(⊥)``` is printed as ```"_" ∧ ¬"_"```.
  Secondly, this ensures that pretty-printing does not produce output such as: ``` ∧ ¬`.
  It is not idiomatic to use this as a generic proposition; use [`@atom`](@ref) instead.

Subtype of [`Language`](@ref).

# Examples
```jldoctest
julia> p = Atom("p")
Atom:
  "p"

julia> p()
Contingency:
  ["p" => ⊤] => ⊤
  ["p" => ⊥] => ⊥
```
"""
struct Atom <: Language
    statement::String

    Atom(statement::String = "") = statement == "" ? new("_") : new(statement)
end

"""
    Literal{
        L <: Union{
            Atom,
            Tuple{Not, Atom}
        }
    } <: Compound <: Language
    Literal(p::L)

An [`Atom`](@ref) or its negation.

Subtype of [`Compound`](@ref) and [`Language`](@ref).
See also [`Not`](@ref).

# Examples
```jldoctest
julia> r = ¬p
Literal:
  ¬"p"

julia> r()
Contingency:
  ["p" => ⊤] => ⊥
  ["p" => ⊥] => ⊤
```
"""
struct Literal{
    L <: Union{
        Atom,
        Tuple{Not, Atom}
    }
} <: Compound
    ϕ::L
end

"""
    Tree{
        L <: Union{
            Tuple{Not, Compound},
            Tuple{And, Compound, Compound}
        }
    } <: Compound <: Language
    Tree(ϕ::L)

Abstract syntax tree representing a compound proposition.

Note that [`Not`](@ref) and [`And`](@ref) are functionally complete operators.

Subtype of [`Compound`](@ref) and [`Language`](@ref).

# Examples
```jldoctest
julia> r = p ∧ ¬p
Tree:
  "p" ∧ ¬"p"

julia> r()
Truth:
  ⊥

julia> (p ∧ q)()
Contingency:
  ["p" => ⊤, "q" => ⊤] => ⊤
  ["p" => ⊤, "q" => ⊥] => ⊥
  ["p" => ⊥, "q" => ⊤] => ⊥
  ["p" => ⊥, "q" => ⊥] => ⊥
```
"""
struct Tree{
    L <: Union{
        Tuple{Not, Compound},
        Tuple{And, Compound, Compound}
    }
} <: Compound
    ϕ::L
end

"""
    Normal{B <: Union{And, Or}} <: Compound <: Language
    Normal(::Union{typeof(and), typeof(or)}, p::Language)

The conjunctive or disjunctive normal form of a proposition.

Constructing an instance with the parameters ```([`and`](@ref), p)``` and ```([`or`](@ref), p)```
correspond to conjunctive and disjunctive normal form, respectively.

Subtype of [`Compound`](@ref) and [`Language`](@ref).

# Examples
```jldoctest
julia> r = Normal(and, p ∧ q)
Normal:
  (¬"p" ∨ "q") ∧ ("p" ∨ ¬"q") ∧ ("p" ∨ "q")

julia> s = Normal(or, ¬p ∨ ¬q)
Normal:
  ("p" ∧ ¬"q") ∨ (¬"p" ∧ "q") ∨ (¬"p" ∧ ¬"q")

julia> t = r ∧ s
Tree:
  (¬"p" ∨ "q") ∧ ("p" ∨ ¬"q") ∧ ("p" ∨ "q") ∧ ("p" ∧ ¬"q") ∨ (¬"p" ∧ "q") ∨ (¬"p" ∧ ¬"q")

julia> t()
Truth:
  ⊥
```
"""
struct Normal{B <: Union{And, Or} #=, L <: Literal =#} <: Compound
    clauses::Vector{Vector{Literal}}
end

"""
    Truth{V <: Union{Val{:contradiction}, Val{:tautology}}} <: Language
    Truth(::V)

Container for the constants [`tautology`](@ref) and [`contradiction`](@ref).
Subtype of [`Language`](@ref).
"""
struct Truth{V <: Union{Val{:contradiction}, Val{:tautology}}} <: Language end

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
const tautology = Truth{Val{:tautology}}()
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
const contradiction = Truth{Val{:contradiction}}()
const ⊥ = contradiction

"""
    Contingency <: Compound

# Examples
```jldoctest
julia> p()
Contingency:
  ["p" => ⊤] => ⊤
  ["p" => ⊥] => ⊥

julia> (p ∧ q)()
Contingency:
  ["p" => ⊤, "q" => ⊤] => ⊤
  ["p" => ⊤, "q" => ⊥] => ⊥
  ["p" => ⊥, "q" => ⊤] => ⊥
  ["p" => ⊥, "q" => ⊥] => ⊥
```
"""
struct Contingency <: Compound # TODO: parameterize
    interpretations::Vector{Pair{Vector{Pair{Atom, Truth}}}}
end


# Utility

"""
    @atom(ps...)

Instantiates [`atomic propositions`](@ref Atom).

Examples
```jldoctest
julia> @atom p q

julia> p
Atom:
  "p"

julia> q
Atom:
  "q"
```
"""
macro atom(expressions...)
    atom = expression -> :($(esc(expression)) = Atom($(string(expression))))
    atoms = map(atom, expressions)

    return :($(atoms...); nothing)
end
#=
Source:
https://github.com/ctrekker/Deductive.jl
=#

"""
    get_atoms(ps::Language...)

Returns a vector of [`atomic propositions`](@ref Atom) contained in ```ps```.

!!! warning
    Some atoms may optimized out of an expression, such as in ```p ∧ ⊥```.

See also [`Language`](@ref).

# Examples
```jldoctest
julia> r = p ∧ q
Tree:
  "p" ∧ "q"

julia> get_atoms(r)
2-element Vector{Atom}:
 "p"
 "q"
```
"""
get_atoms(::Truth) = Atom[]
get_atoms(p::Contingency) = union(
    mapreduce(
        interpretation -> map(
            literal -> first(literal), first(interpretation)),
        vcat, p.interpretations
    )
)
get_atoms(p::Atom) = [p]
get_atoms(p::Literal) = get_atoms(p.ϕ)
get_atoms(p::Tree) = union(get_atoms(p.ϕ))
get_atoms(ϕ::Tuple{Operator, Vararg}) = mapreduce(p -> get_atoms(p), vcat, Base.tail(ϕ))
get_atoms(p::Normal) = get_atoms(Tree(p))
get_atoms(ps::Language...) = union(mapreduce(get_atoms, vcat, ps))


# Helpers 

convert(::Type{Literal}, literal::Pair{Atom, <:Truth}) = last(literal) == tautology ? Literal(first(literal)) : not(first(literal))
convert(::Type{Tree}, p::typeof(⊥)) = and(Atom(), not(Atom()))
convert(::Type{Tree}, p::typeof(⊤)) = not(Tree(contradiction))
convert(::Type{Tree}, p::Contingency) = mapreduce(interpretation -> mapreduce(Literal, and, first(interpretation)), or, filter(interpretation -> last(interpretation) == ⊤, p.interpretations))
convert(n::Type{<:Normal}, p::Contingency) = n(Tree(p))
convert(::Type{Tree}, p::Normal{And}) = _convert(p, or, and)
convert(::Type{Tree}, p::Normal{Or}) = _convert(p, and, or)
convert(::Type{Contingency}, p::Language) = p()
convert(::Type{L}, p::L) where L <: Language = p
convert(::Type{L}, p::Language) where L <: Language = L(p)

_convert(p, inner, outer) = mapreduce(clause -> reduce(inner, clause), outer, p.clauses)


# Consructors

# TODO: use Base.promote?
Tree(::Not, p::Atom) = Literal((Not(), p))
Tree(::Not, p::Compound) = Tree((Not(), p))

Tree(::And, p::Atom, q::Atom) = Tree(And(), Literal(p), Literal(q))
Tree(::And, p::Atom, q::Compound) = Tree(And(), Literal(p), q)
Tree(::And, p::Compound, q::Atom) = Tree(And(), p, Literal(q))
Tree(::And, p::Compound, q::Compound) = Tree((And(), p, q))

# TODO: write more conversions
Literal(p::Pair{Atom, Truth}) = convert(Literal, p)
Tree(p::Language) = convert(Tree, p)
Normal(::B, p::Contingency) where B <: Union{And, Or} = convert(Normal{B}, p)

Normal(::And, p::Language) = not(Normal(Or(), ¬p))
function Normal(::Or, p::Language)
    q = p()
    # TODO: change `===` to `==` - fixes `Normal(and, ⊥)`
    interpretations =
        if q === ⊤
            [[Atom() => ⊤], [Atom() => ⊥]]
        elseif q === ⊥
            [[Atom() => ⊤, Atom() => ⊥]]
        else
            map(first, filter(literal -> last(literal) == ⊤, q.interpretations))
        end

    clauses = map(interpretation -> map(Literal, interpretation), interpretations)
    return Normal{Or}(clauses)
end
