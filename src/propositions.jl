
using InteractiveUtils

"""
    Proposition

The set of [well-formed logical formulae](https://en.wikipedia.org/wiki/Well-formed_formula).

Supertype of [`Atom`](@ref) and [`Compound`](@ref).
"""
abstract type Proposition end

"""
    Compound <: Proposition

A proposition composed from connecting [`Atom`](@ref)icpropositions with [`BooleanOperator`](@ref)s.

Subtype of [`Proposition`](@ref).
Supertype of [`Literal`](@ref), [`Clause`](@ref), and [`Expressive`](@ref).
"""
abstract type Compound <: Proposition end

"""
    Expressive <: Compound

A proposition that is [expressively complete](https://en.wikipedia.org/wiki/Completeness_(logic)).

Subtype of [`Compound`](@ref).
Supertype of [`Valuation`](@ref), [`Tree`](@ref), and[`Normal`](@ref).
"""
abstract type Expressive <: Compound end

"""
    Atom{SS <: Union{String, Symbol}} <: Proposition
    Atom(::SS = :_)
    Atom(::AtomicProposition)

A proposition with [no deeper propositional structure](https://en.wikipedia.org/wiki/Atomic_formula).

A string argument can be thought of as a specific statement,while a symbol can be variable.
However, the only builtin difference between these are how they pretty-print.
An atom with a string argument will be encompassed by quotation marks,
while an atom with a symbol argument will only show the symbol's characters.

!!! tip
    Use [`@atoms`](@ref) or [`@p`](@ref) as a shortcut to
    define atoms or instantiate them inline, respectively.

!!! info
    The default parameter `:_` represents an Atom with an unspecified statement.
    For example, `Tree(⊥)` returns `Tree(and(Atom(:_), not(Atom(:_)))`,
    which pretty-prints as `_ ∧ ¬_`.
    The underscore is a special case; it is not idiomatic to use for most purposes.

Subtype of [`Proposition`](@ref).
See also [`AtomicProposition`](@ref).

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

    Atom(statement::SS = :_) where SS <: Union{Symbol, String} = new{SS}(statement)
end

"""
    Literal{UO <: UnaryOperator} <: Compound
    Literal(::UO, ::Atom)
    Literal(::LiteralProposition)

A proposition represented by [an atomic formula or its negation]
(https://en.wikipedia.org/wiki/Literal_(mathematical_logic)).

Subtype of [`Compound`](@ref).
See also [`UnaryOperator`](@ref), [`Atom`](@ref), and [`LiteralProposition`](@ref).

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

    Literal(::UO, atom::Atom) where UO <: UnaryOperator = new{UO}(atom)
end

"""
    Clause{AO <: AndOr, L <: Literal} <: Compound
    Clause(::AO, ::Vector = Literal[])
    Clause(::AO, ps...)

A proposition represented as either a [conjunction or disjunction of literals]
(https://en.wikipedia.org/wiki/Clause_(logic)).

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

    Clause(::AO, literals::Vector{L} = Literal[]) where {AO <: AndOr, L <: Literal} =
        new{AO, L}(union(literals))
end

"""
    Normal{AO <: AndOr, C <: Clause} <: Expressive
    Normal(::A, ::Vector{C} = C[]) where {A <: typeof(and), C <: Clause{typeof(or)}}
    Normal(::O, ::Vector{C} = C[]) where {O <: typeof(or), C <: Clause{typeof(and)}}
    Normal(::AO, ps...)

A proposition represented in [conjunctive](https://en.wikipedia.org/wiki/Conjunctive_normal_form)
or [disjunctive](https://en.wikipedia.org/wiki/Disjunctive_normal_form) normal form.

!!! info
    An empty normal form is logically equivalent to the
    [`identity`](@ref) element of it's binary operator.

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

    Normal(::A, clauses::Vector{C} = C[]) where {A <: typeof(and), C <: Clause{typeof(or)}} =
        new{A, C}(union(clauses))
    Normal(::O, clauses::Vector{C} = C[]) where {O <: typeof(or), C <: Clause{typeof(and)}} =
        new{O, C}(union(clauses))
end

"""
    Valuation{P <: Pair} <: Expressive
    Valuation(::Vector{P})
    Valuation(p)

Proposition represented by a vector of [interpretations]
(https://en.wikipedia.org/wiki/Interpretation_(logic)).

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

    function Valuation(interpretations::Vector{P}) where P <: Pair # {Vector{Pair{<:Atom, <:Truth}}, <:Truth}}
        isempty(interpretations) && error("TODO: write this exception")
        return new{P}(union(interpretations))
    end
end

"""
    Tree{
        O <: BooleanOperator,
        P <: Union{Tuple{Proposition}, Tuple{Proposition, Proposition}}
    } <: Expressive
    Tree(::UnaryOperator, ::Atom)
    Tree(::BinaryOperator, ::Tree, ::Tree)
    Tree(x)

A proposition represented by an [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree).

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

    Tree(::UO, leaf_node::A) where {UO <: UnaryOperator, A <: Atom} =
        new{UO, Tuple{A}}((leaf_node,))
    Tree(::BO, left_node::T1, right_node::T2) where {BO <: BinaryOperator, T1 <: Tree, T2 <: Tree} =
        new{BO, Tuple{T1, T2}}((left_node, right_node))
end

get_concrete_types(type::UnionAll) = type
get_concrete_types(type::DataType) = mapreduce(get_concrete_types, vcat, subtypes(type))

get_abstract_types(type::UnionAll) = []
get_abstract_types(type::DataType) = mapreduce(get_abstract_types, vcat, subtypes(type), init = type)

const concrete_propositions = get_concrete_types(Proposition)
const abstract_propositions = get_abstract_types(Proposition)

"""
    AtomicProposition

A [`Proposition`](@ref) that is known by its type to be equivalent to an [`Atom`](@ref).
"""
const AtomicProposition = Union{Atom, Literal{typeof(identity)}, Tree{typeof(identity)}}

"""
    LiteralProposition

A [`Proposition`](@ref) that is known by its type to be equivalent to a [`Literal`](@ref).
"""
const LiteralProposition = Union{
    Base.uniontypes(AtomicProposition)...,
    Literal{typeof(not)},
    Tree{typeof(not), <:Tuple{Tree{typeof(identity), <:Tuple{Atom}}}}
}

"""
    NonExpressive

A [`Proposition`](@ref) that is not an [`Expressive`](@ref).
"""
const NonExpressive = Union{filter(concrete_propositions) do type
    !(type <: Expressive)
end...}
# TODO: make traits?
