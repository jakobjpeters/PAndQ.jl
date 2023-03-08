
using InteractiveUtils

"""
    Proposition

The set of [well-formed logical formulae](https://en.wikipedia.org/wiki/Well-formed_formula).

Supertype of [`Atom`](@ref) and [`Compound`](@ref).
"""
abstract type Proposition end

"""
    Compound <: Proposition

A proposition composed from one or more [`Atom`](@ref)ic propositions.

Subtype of [`Proposition`](@ref).
Supertype of [`Literal`](@ref), [`Clause`](@ref), and [`Expressive`](@ref).
"""
abstract type Compound <: Proposition end

"""
    Expressive <: Compound

A proposition that is [expressively complete](https://en.wikipedia.org/wiki/Completeness_(logic)).

Subtype of [`Compound`](@ref).
Supertype of [`Valuation`](@ref), [`Tree`](@ref), [`Normal`](@ref), and [`Pretty`](@ref).
"""
abstract type Expressive <: Compound end

"""
    Atom{SS <: Union{String, Symbol}} <: Proposition
    Atom([::Union{Symbol, String])
    Atom(::Proposition)

A proposition with [no deeper propositional structure](https://en.wikipedia.org/wiki/Atomic_formula).

A string argument can be thought of as a specific statement,
while a symbol can be variable. However, the only builtin difference
between these are how they pretty-print. An atom with a string argument will
be encompassed by quotation marks, while an atom with a symbol argument will
only show the symbol's characters.

!!! info
    `Atom()` defaults to `Atom(:_)`. This underscore symbol is useful as a
    default, such as when representing a truth value. For example,
    `Tree(⊥)` is pretty-printed as `_ ∧ ¬_`. This is a special case; it is not
    idiomatic to use for most purposes.

Subtype of [`Proposition`](@ref).

# Examples
```jldoctest
julia> Atom(:p)
Atom:
 p

julia> Atom("Logic is fun")
Atom:
 "Logic is fun"
```
"""
struct Atom{SS <: Union{String, Symbol}} <: Proposition
    statement::SS

    Atom(p::SS = :_) where SS <: Union{Symbol, String} = new{SS}(p)
end

"""
    Literal{UO <: UnaryOperator} <: Compound
    Literal(::UO, ::Atom)
    Literal(::Proposition)

A proposition represented by
[an atomic formula or its negation](https://en.wikipedia.org/wiki/Literal_(mathematical_logic)).

Subtype of [`Compound`](@ref).
See also [`UnaryOperator`](@ref) and [`Atom`](@ref).

# Examples
```jldoctest
julia> r = @p ¬p
Literal:
 ¬p

julia> ¬r
Atom:
 p
```
"""
struct Literal{UO <: UnaryOperator} <: Compound
    atom::Atom

    Literal(::UO, p::Atom) where UO <: UnaryOperator = new{UO}(p)
end

"""
    Clause{
        AO <: AndOr,
        L <: Literal
    } <: Compound
    Clause(::AO, [ps::Vector])
    Clause(::AO, ps...)

A proposition represented as either a
[conjunction or disjunction of literals](https://en.wikipedia.org/wiki/Clause_(logic).

!!! info
    An empty clause is logically equivalent to the [`identity`](@ref) element of it's binary operator.

See also [`Literal`](@ref).
Subtype of [`Compound`](@ref).

# Examples
```
julia> Clause(and)
Clause:
 ⊥

julia> @p Clause(and, p, q)
Clause:
 p ∧ q

julia> @p Clause(or, [¬p, q])
Clause:
 ¬p ∨ q
```
"""
struct Clause{AO <: AndOr, L <: Literal} <: Compound
    literals::Vector{L}

    Clause(::AO, ps::Vector{L} = Literal[]) where {AO <: AndOr, L <: Literal} = new{AO, L}(union(ps))
end

"""
    Normal{AO <: AndOr, C <: Clause} <: Expressive
    Normal(::typef(and), ::Vector{Clause{typeof(or)}})
    Normal(::typef(or), ::Vector{Clause{typeof(and)}})
    Normal(p)

A proposition represented in [conjunctive](https://en.wikipedia.org/wiki/Conjunctive_normal_form)
or [disjunctive](https://en.wikipedia.org/wiki/Disjunctive_normal_form) normal form.

!!! info
    An empty normal form is logically equivalent to the [`identity`](@ref) element of it's binary operator.

Subtype of [`Expressive`](@ref).

# Examples
```jldoctest
julia> s = @p Normal(and, Clause(or, p, q), Clause(or, ¬r))
Normal:
 (p ∨ q) ∧ (¬r)

julia> ¬s
Normal:
 (¬p ∧ ¬q) ∨ (r)
```
"""
struct Normal{AO <: AndOr, C <: Clause} <: Expressive
    clauses::Vector{C}

    Normal(::A, ps::Vector{<:Clause{typeof(or)}} = Clause{typeof(or)}[]) where A <: typeof(and) = new{A, eltype(ps)}(union(ps))
    Normal(::O, ps::Vector{<:Clause{typeof(and)}} = Clause{typeof(and)}[]) where O <: typeof(or) = new{O, eltype(ps)}(union(ps))
end

"""
    Interpretation

# Examples
"""
# struct Interpretation{C <: Clause{typeof(and)}, T <: Truth} <: Proposition
#     p::C
#     q::T
# end
# not(p::Interpretation) = Interpretation(p.p, not(p.q))
# is_tautology(p::Interpretation{typeof(⊤)}) = true
# is_tautology(p::Interpretation{typeof(⊥)}) = false
# solve(p::Valuation) = map(x -> x.p, filter(is_tautology, p.p))
# struct Valuation{I <: Interpretation} <: Expressive
#     p::Vector{I}
# end

"""
    Valuation{P <: Pair} <: Expressive
    Valuation(::Vector{P})
    Valuation(p)

Proposition represented by a vector of
[interpretations](https://en.wikipedia.org/wiki/Interpretation_(logic)).

Subtype of [`Expressive`](@ref).

# Examples
```jldoctest
julia> @p Valuation(p ∧ q)
Valuation:
 [p => ⊤, q => ⊤] => ⊤
 [p => ⊥, q => ⊤] => ⊥
 [p => ⊤, q => ⊥] => ⊥
 [p => ⊥, q => ⊥] => ⊥

julia> Valuation(⊥)
Valuation:
 [] => ⊥
```
"""
struct Valuation{P <: Pair} <: Expressive # {<:Vector{<:Pair{<:Atom, <:Truth}}, <:Truth}} <: Expressive
    interpretations::Vector{P}

    function Valuation(p::Vector{P}) where P <: Pair # {Vector{Pair{<:Atom, <:Truth}}, <:Truth}}
        isempty(p) && error("TODO: write this exception")
        return new{P}(union(p))
    end
end

"""
    Tree{
        O <: BooleanOperator,
        P <: Union{Proposition, Tuple{Proposition, Proposition}}
    } <: Expressive
    Tree(::UnaryOperator, ::Atom)
    Tree(::BinaryOperator, ::Tree, ::Tree)
    Tree(p)

Proposition represented by an [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree).

Subtype of [`Expressive`](@ref).

# Examples
```jldoctest
julia> r = @p p ⊻ q
Tree:
 p ⊻ q

julia> @p ¬r → s
Tree:
 (p ↔ q) → s
```
"""
struct Tree{
    BO <: BooleanOperator,
    TP <: Union{Tuple{Proposition}, Tuple{Proposition, Proposition}}
} <: Expressive
    node::TP

    Tree(::UO, p::A) where {UO <: UnaryOperator, A <: Atom} = new{UO, Tuple{A}}((p,))
    Tree(::BO, p::T1, q::T2) where {BO <: BinaryOperator, T1 <: Tree, T2 <: Tree} = new{BO, Tuple{T1, T2}}((p, q))
end

get_concrete_types(type::UnionAll) = type
get_concrete_types(type::DataType) = mapreduce(get_concrete_types, vcat, subtypes(type))

get_abstract_types(type::UnionAll) = []
get_abstract_types(type::DataType) = mapreduce(get_abstract_types, vcat, subtypes(type), init = type)

const concrete_propositions = get_concrete_types(Proposition)
const abstract_propositions = get_abstract_types(Proposition)

const AtomicProposition = Union{Atom, Literal{typeof(identity)}, Tree{typeof(identity)}}
const LiteralProposition = Union{
    Base.uniontypes(AtomicProposition)...,
    Literal{typeof(not)},
    Tree{typeof(not), <:Tuple{Tree{typeof(identity), <:Tuple{Atom}}}}
}
const NonExpressive = Union{setdiff(abstract_propositions, [Proposition, Expressive])...}
# TODO: make traits?
