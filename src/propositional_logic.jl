
# Types

"""
    Proposition

Set of well-formed logical formulae.

!!! tip
    Calling a ```Proposition```s redirects to [`interpret`](@ref).

Supertype of [`Truth`](@ref), [`Atom`](@ref), [`Literal`](@ref),
[`Compound`](@ref), [`Tree`](@ref), [`Valuation`](@ref),
[`Clause`](@ref), [`Normal`](@ref), and [`Pretty`](@ref).
```
"""
abstract type Proposition end

"""
Compound <: Proposition

Compound proposition.

Subtype of [`Proposition`](@ref).
Supertype of [`Literal`](@ref), [`Tree`](@ref), [`Valuation`](@ref),
[`Clause`](@ref), [`Normal`](@ref), and [`Pretty`](@ref).
"""
abstract type Compound <: Proposition end

"""
    Expressive <: Compound <: Proposition

All subtypes of ```Expressive``` are guaranteed to be
[expressively complete](https://en.wikipedia.org/wiki/Completeness_(logic)).

Supertype of [`Tree`](@ref), [`Valuation`](@ref), and [`Normal`](@ref).
Subtype of [`Compound`](@ref) and [`Proposition`](@ref).
"""
abstract type Expressive <: Compound end

"""
    Operator

Set of functions that operate on a logical [`Proposition`](@ref).

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
    Atom <: Proposition
    Atom([p::String])

Atomic proposition.

!!! info
    Constructing an ```Atom``` with no argument, an empty string, or an underscore character
    will set ```p = "_"```. This serves two purposes.
    Firstly, it is useful as a default proposition when converting [`Truth`](@ref)s to other forms;
    for example: ```Tree(⊥)``` is printed as ```"_" ∧ ¬"_"```.
    Secondly, this ensures that pretty-printing does not produce output such as: ``` ∧ ¬`.
    It is not idiomatic to use this as a generic proposition; use [`@atom`](@ref) instead.

Subtype of [`Proposition`](@ref).

# Examples
```jldoctest
julia> p = Atom("p")
Atom:
  "p"

julia> Valuation(p)
Valuation:
  ["p" => ⊤] => ⊤
  ["p" => ⊥] => ⊥
```
"""
struct Atom <: Proposition
    p::String

    Atom(p::String = "") = p == "" ? new("_") : new(p)
end

"""
    Literal{
        L <: Union{
            Atom,
            Tuple{Not, Atom}
        }
    } <: Compound <: Proposition
    Literal(p::L)

An [`Atom`](@ref) or its negation.

Subtype of [`Compound`](@ref) and [`Proposition`](@ref).
See also [`Not`](@ref).

# Examples
```jldoctest
julia> r = ¬p
Literal:
  ¬"p"

julia> Valuation(r)
Valuation:
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
    p::L
end

"""
    Tree{
        NA <: Union{
            Tuple{Not, Compound},
            Tuple{And, Compound, Compound}
        }
    } <: Expressive <: Compound <: Proposition
    Tree(p::NA)

Abstract syntax tree representing a proposition.

Note that [`Not`](@ref) and [`And`](@ref) are functionally complete operators.

Subtype of [`Expressive`](@ref), [`Compound`](@ref) and [`Proposition`](@ref).

# Examples
```jldoctest
julia> r = p ∧ ¬p
Tree:
  "p" ∧ ¬"p"

julia> Valuation(r)
Valuation:
  ["p" => ⊤] => ⊥
  ["p" => ⊥] => ⊥

julia> Valuation(p ∧ q)
Valuation:
  ["p" => ⊤, "q" => ⊤] => ⊤
  ["p" => ⊤, "q" => ⊥] => ⊥
  ["p" => ⊥, "q" => ⊤] => ⊥
  ["p" => ⊥, "q" => ⊥] => ⊥
```
"""
struct Tree{
    NA <: Union{
        Tuple{Not, Compound},
        Tuple{And, Compound, Compound}
    }
} <: Expressive
    p::NA
end

"""
    Clause{
        B <: Union{And, Or},
        VL <: Vector{<:Literal}
    } <: Compound <: Proposition
    Clause()

Empty clauses are always false.
```Clause(::B) where B <: Union{And, Or}``` is automatically converted to ```Clause(::B, ⊥)```
for readability.

```
julia>

```
"""
struct Clause{
    B <: Union{And, Or}
    # VL <: Vector{Literal}
} <: Compound
    p::Vector{Literal}

    Clause(::B, ps) where B = new{B}(unique(p -> convert(Literal, p), ps))
end

"""
    Normal{B <: Union{And, Or}} <: Expressive <: Compound <: Proposition
    Normal(::Union{typeof(and), typeof(or)}, p::Proposition)

The conjunctive or disjunctive normal form of a proposition.

Constructing an instance with the parameters ```([`and`](@ref), p)``` and ```([`or`](@ref), p)```
correspond to conjunctive and disjunctive normal form, respectively.

Subtype of [`Expressive`](@ref), [`Compound`](@ref) and [`Proposition`](@ref).

# Examples
```jldoctest
julia> r = Normal(PAQ.And(), p ∧ q)
Normal:
  (¬"p" ∨ "q") ∧ ("p" ∨ ¬"q") ∧ ("p" ∨ "q")

julia> s = Normal(PAQ.Or(), ¬p ∨ ¬q)
Normal:
  ("p" ∧ ¬"q") ∨ (¬"p" ∧ "q") ∨ (¬"p" ∧ ¬"q")
```
"""
struct Normal{B <: Union{And, Or}, VC <: Vector{<:Clause}} <: Expressive
    p::VC

    function Normal(::AO1, clauses::Vector{Clause{AO2}}) where {AO1 <: Union{And, Or}, AO2 <: Union{And, Or}}
        new{AO1, Vector{Clause{AO2}}}(clauses)
    end
    function Normal(::AO, clauses::Vector{Clause{AO}}) where AO <: Union{And, Or}
        new{AO, Vector{Clause{AO}}}(mapreduce(clause -> map(p -> Clause(AO(), p), clause.p), vcat, clauses))
    end
end

"""
    Truth{V <: Union{Val{:⊥}, Val{:⊤}}} <: Proposition
    Truth(::V)

Container for the constants [`tautology`](@ref) and [`contradiction`](@ref).
Subtype of [`Proposition`](@ref).
"""
struct Truth{V <: Union{Val{:⊥}, Val{:⊤}}} <: Proposition end

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
    Valuation <: Expressive <: Compound <: Proposition

Subtype of [`Expressive`](@ref), [`Compound`](@ref) and [`Proposition`](@ref).

# Examples
```jldoctest
julia> Valuation(p)
Valuation:
  ["p" => ⊤] => ⊤
  ["p" => ⊥] => ⊥

julia> Valuation(p ∧ q)
Valuation:
  ["p" => ⊤, "q" => ⊤] => ⊤
  ["p" => ⊤, "q" => ⊥] => ⊥
  ["p" => ⊥, "q" => ⊤] => ⊥
  ["p" => ⊥, "q" => ⊥] => ⊥
```
"""
struct Valuation <: Expressive # TODO: parameterize
    p::Vector{Pair{Vector{Pair{Atom, Truth}}}}
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
    get_atoms(ps::Proposition...)

Returns a vector of [`atomic propositions`](@ref Atom) contained in ```ps```.

!!! warning
    Some atoms may optimized out of an expression, such as in ```p ∧ ⊥```.

See also [`Proposition`](@ref).

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
get_atoms(p::Valuation) = union(
    mapreduce(
        interpretation -> map(
            literal -> first(literal), first(interpretation)),
        vcat, p.p
    )
)
get_atoms(p::Atom) = [p]
get_atoms(p::Literal) = get_atoms(p.p)
get_atoms(p::Tree) = union(get_atoms(p.p))
get_atoms(node::Tuple{Operator, Vararg}) = mapreduce(p -> get_atoms(p), vcat, Base.tail(node))
get_atoms(p::Normal) = get_atoms(Tree(p))
get_atoms(ps::Proposition...) = union(mapreduce(get_atoms, vcat, ps))
# TODO: try `unique`?
