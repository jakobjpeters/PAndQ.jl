
"""
    Proposition

The set of [well-formed logical formulae](https://en.wikipedia.org/wiki/Well-formed_formula).

Supertype of [`Atom`](@ref) and [`Compound`](@ref).
"""
abstract type Proposition end

"""
    Compound <: Proposition

A proposition composed from connecting [`Atom`](@ref)icpropositions with [`LogicalOperator`](@ref)s.

Subtype of [`Proposition`](@ref).
Supertype of [`Literal`](@ref), [`Clause`](@ref), and [`Expressive`](@ref).
"""
abstract type Compound <: Proposition end

"""
    Expressive <: Compound

A proposition that is [expressively complete](https://en.wikipedia.org/wiki/Completeness_(logic)).

Subtype of [`Compound`](@ref).
Supertype of [`Tree`](@ref) and [`Normal`](@ref).
"""
abstract type Expressive <: Compound end

"""
    Atom{T} <: Proposition
    Atom(::T = :_)
    Atom(::AtomicProposition)

A proposition with [no deeper propositional structure](https://en.wikipedia.org/wiki/Atomic_formula).

!!! tip
    Define pretty-printing for an instance of `Atom{T}` by overloading
    [`show(io::IO, p::Atom{T})`](@ref show).

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
struct Atom{T} <: Proposition
    statement::T

    Atom(statement::T = :_) where T = new{T}(statement)
end

"""
    Literal{UO <: UnaryOperator, T} <: Compound
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
struct Literal{UO <: UnaryOperator, T} <: Compound
    atom::Atom{T}

    Literal(::UO, atom::Atom{T}) where {UO <: UnaryOperator, T} = new{UO, T}(atom)
end

"""
    Tree{
        LO <: LogicalOperator,
        P <: Union{Tuple{Proposition}, Tuple{Proposition, Proposition}}
    } <: Expressive
    Tree(::UnaryOperator, ::Atom)
    Tree(::BinaryOperator, ::Tree, ::Tree)
    Tree(x)

A proposition represented by an [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree).

Subtype of [`Expressive`](@ref).

# Examples
```jldoctest
julia> @p r = p ⊻ q
Tree:
 p ⊻ q

julia> @p ¬r → s
Tree:
 (p ↔ q) → s
```
"""
struct Tree{
    LO <: LogicalOperator,
    NT <: NTuple{N, Proposition} where N
} <: Expressive
    nodes::NT

    Tree(::UO, node::A) where {UO <: UnaryOperator, A <: Atom} = new{UO, Tuple{A}}((node,))
    Tree(lo::LO, nodes...) where LO <: LogicalOperator = begin
        _arity = lo |> arity
        _arity != nodes |> length && "TODO: write this error" |> error
        new{LO, NTuple{_arity, Tree}}(nodes)
    end
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
        new{AO, L}(literals |> union)
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

    Normal(::A, clauses::Vector{C} = Clause{typeof(or)}[]) where {A <: typeof(and), C <: Clause{typeof(or)}} =
        new{A, C}(clauses |> union)
    Normal(::O, clauses::Vector{C} = Clause{typeof(and)}[]) where {O <: typeof(or), C <: Clause{typeof(and)}} =
        new{O, C}(clauses |> union)
end

"""
    AtomicProposition

A [`Proposition`](@ref) that is known by its type to be logically equivalent to an [`Atom`](@ref).
"""
const AtomicProposition = Union{
    Atom,
    Literal{identity |> typeof},
    Tree{identity |> typeof, Tuple{<:Atom}}
}

"""
    LiteralProposition

A [`Proposition`](@ref) that is known by its type to be logically equivalent to a [`Literal`](@ref).
"""
const LiteralProposition = Union{
    AtomicProposition,
    Literal{not |> typeof},
    Tree{not |> typeof, Tuple{<:Atom}}
}

# TODO: make traits?
