
import AbstractTrees: children, printnode
using AbstractTrees: childtype, Leaves

# Abstract Types

"""
    Proposition

The set of [well-formed logical formulae](https://en.wikipedia.org/wiki/Well-formed_formula).

Supertype of [`Atom`](@ref) and [`Compound`](@ref).
"""
abstract type Proposition end

"""
    Compound{LO} <: Proposition

A proposition composed from connecting [`Atom`](@ref)ic propositions with [`LogicalOperator`](@ref)s.

Subtype of [`Proposition`](@ref).
Supertype of [`Literal`](@ref), [`Clause`](@ref), and [`Expressive`](@ref).
"""
abstract type Compound{LO} <: Proposition end

"""
    Expressive{LO} <: Compound{LO}

A proposition that is [expressively complete](https://en.wikipedia.org/wiki/Completeness_(logic)).

Subtype of [`Compound`](@ref).
Supertype of [`Tree`](@ref) and [`Normal`](@ref).
"""
abstract type Expressive{LO} <: Compound{LO} end

# Concrete Types

"""
    Atom{T} <: Proposition
    Atom(::T)

A proposition with [no deeper propositional structure](https://en.wikipedia.org/wiki/Atomic_formula).

!!! tip
    Define pretty-printing for an instance of `Atom{T}` by overloading
    [`show(io::IO, ::MIME"text/plain", p::Atom{T})`](@ref show).

!!! tip
    Use [`@atoms`](@ref) and [`@p`](@ref) as shortcuts to
    define atoms or instantiate them inline, respectively.

Subtype of [`Proposition`](@ref).

# Examples
```jldoctest
julia> Atom(:p)
p

julia> Atom("Logic is fun")
Atom("Logic is fun")
```
"""
struct Atom{T} <: Proposition
    statement::T
end

"""
    Literal{UO <: UnaryOperator, T} <: Compound{UO}
    Literal(::UO, ::Atom{T})
    Literal(::LiteralProposition)

A proposition represented by [an atomic formula or its negation]
(https://en.wikipedia.org/wiki/Literal_(mathematical_logic)).

Subtype of [`Compound`](@ref).
See also [`UnaryOperator`](@ref), [`Atom`](@ref), and [`LiteralProposition`](@ref).

# Examples
```jldoctest
julia> r = @p ¬p
¬p

julia> ¬r
p
```
"""
struct Literal{UO <: UnaryOperator, T} <: Compound{UO}
    atom::Atom{T}

    Literal(::UO, atom::Atom{T}) where {UO <: UnaryOperator, T} = new{UO, T}(atom)
end

"""
    Tree{LO <: LogicalOperator, AT <: Union{Atom, Tree}} <: Expressive{LO}
    Tree(::NullaryOperator, ::Atom)
    Tree(::LogicalOperator, ::Tree...)
    Tree(::Proposition)

A proposition represented by an [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree).

Subtype of [`Expressive`](@ref).
See also [`LogicalOperator`](@ref).

# Examples
```jldoctest
julia> @p r = p ⊻ q
p ⊻ q

julia> @p ¬r → s
(p ↔ q) → s
```
"""
struct Tree{LO <: LogicalOperator, P <: Proposition} <: Expressive{LO}
    nodes::Vector{P}

    Tree(::NO) where NO <: NullaryOperator = new{NO, Tree}([])
    Tree(::UO, node::A) where {UO <: UnaryOperator, A <: Atom} = new{UO, A}([node])
    function Tree(lo::LO, nodes::Tree...) where LO <: LogicalOperator
        @assert arity(lo) == length(nodes)
        new{LO, childtype(nodes)}(collect(nodes))
    end
end

"""
    Clause{AO <: AndOr, L <: Literal} <: Compound{AO}
    Clause(::AO, ps = Literal[])
    Clause(::AO, p::Proposition)
    Clause(::Union{NullaryOperator, LiteralProposition})

A proposition represented as either a [conjunction or disjunction of literals]
(https://en.wikipedia.org/wiki/Clause_(logic)).

!!! info
    An empty clause is logically equivalent to the
    neutral element of it's binary operator.

Subtype of [`Compound`](@ref).
See also [`AndOr`](@ref), [`Literal`](@ref),
[`NullaryOperator`](@ref), and [`LiteralProposition`](@ref).

# Examples
```jldoctest
julia> Clause(and)
⊤

julia> @p Clause(p)
p

julia> @p Clause(or, [¬p, q])
¬p ∨ q
```
"""
struct Clause{AO <: AndOr, L <: Literal} <: Compound{AO}
    literals::Vector{L}

    Clause(::AO, literals::Vector{L} = Literal[]) where {AO <: AndOr, L <: Literal} =
        new{AO, L}(union(literals))
end

"""
    Normal{AO <: AndOr, C <: Clause} <: Expressive{AO}
    Normal(::typeof(and), ps = Clause{typeof(or)}[])
    Normal(::typeof(or), ps = Clause{typeof(and)}[])
    Normal(::AO, ::Proposition)
    Normal(::Union{NullaryOperator, Proposition})

A proposition represented in [conjunctive](https://en.wikipedia.org/wiki/Conjunctive_normal_form)
or [disjunctive](https://en.wikipedia.org/wiki/Disjunctive_normal_form) normal form.

!!! info
    An empty normal form is logically equivalent to the
    neutral element of it's binary operator.

Subtype of [`Expressive`](@ref).
See also [`AndOr`](@ref), [`Clause`](@ref),
[`NullaryOperator`](@ref), and [`Proposition`](@ref).

# Examples
```jldoctest
julia> s = @p Normal(and, [Clause(or, [p, q]), Clause(or, ¬r)])
(p ∨ q) ∧ (¬r)

julia> ¬s
(¬p ∧ ¬q) ∨ (r)
```
"""
struct Normal{AO <: AndOr, C <: Clause} <: Expressive{AO}
    clauses::Vector{C}

    Normal(::A, clauses::Vector{C} = Clause{typeof(or)}[]) where {A <: typeof(and), C <: Clause{typeof(or)}} =
        new{A, C}(union(clauses))
    Normal(::O, clauses::Vector{C} = Clause{typeof(and)}[]) where {O <: typeof(or), C <: Clause{typeof(and)}} =
        new{O, C}(union(clauses))
end

# Internals

## Union Types

"""
    AtomicProposition

A [`Proposition`](@ref) that is known by its type to be logically equivalent to an [`Atom`](@ref).
"""
const AtomicProposition = Union{
    Atom,
    Literal{typeof(identity)},
    Tree{typeof(identity), <:Atom}
}

"""
    LiteralProposition

A [`Proposition`](@ref) that is known by its type to be logically equivalent to a [`Literal`](@ref).
"""
const LiteralProposition = Union{
    AtomicProposition,
    Literal{typeof(not)},
    Tree{typeof(not), <:Atom}
}

## AbstractTrees.jl

"""
    children(::Proposition)

Return an iterator over the leaf nodes of the given [`Proposition`](@ref).

!!! warning
    Nodes that apply the [`identity`](@ref) function are skipped.

# Examples
```jldoctest
julia> @p PAndQ.children(Literal(identity, p))
()

julia> @p PAndQ.children(Literal(not, p))
(Atom(:p),)

julia> @p PAndQ.children(p ⊻ q)
2-element Vector{Tree{typeof(identity), Atom{Symbol}}}:
 p
 q

julia> @p PAndQ.children(Normal(and, p ⊻ q))
2-element Vector{Clause{typeof(or)}}:
 p ∨ q
 ¬p ∨ ¬q
```
"""
children(p::Literal{typeof(not)}) = (p.atom,)
children(p::Tree{typeof(identity), <:Atom}) = ()
children(p::Tree{typeof(identity), <:Tree}) = children(only(p.nodes))
children(p::Union{Tree, Clause, Normal}) = only_field(p)

"""
    printnode(::IO, ::Proposition)

!!! warning
    Nodes that apply the [`identity`](@ref) function are skipped.

# Examples
```jldoctest
julia> @p PAndQ.printnode(stdout, Literal(identity, p))
p
julia> @p PAndQ.printnode(stdout, Literal(not, p))
¬
julia> @p PAndQ.printnode(stdout, p ⊻ q)
⊻
julia> @p PAndQ.printnode(stdout, Normal(and, p ⊻ q))
∧
```
"""
printnode(io::IO, p::Atom; kwargs...) = show(io, MIME"text/plain"(), p)
printnode(io::IO, p::Literal{typeof(identity)}; kwargs...) = printnode(io, p.atom; kwargs...)
printnode(io::IO, p::Tree{typeof(identity)}; kwargs...) = printnode(io, only(p.nodes); kwargs...)
printnode(io::IO, p::Compound{LO}; kwargs...) where LO =
    print(io, operator_to_symbol(LO.instance))

## Utilities

"""
    union_all_type(::Proposition)

Return the `UnionAll` type of a [`Proposition`](@ref).

# Examples
```jldoctest
julia> @p PAndQ.union_all_type(p)
Atom

julia> @p PAndQ.union_all_type(p ∧ q)
Tree
```
"""
function union_all_type end
for T in (:Atom, :Literal, :Tree, :Clause, :Normal)
    @eval union_all_type(::$T) = $T
end

"""
    only_field(::Proposition)

Return the only field of a [`Proposition`](@ref).

# Examples
```jldoctest
julia> @p PAndQ.only_field(p)
:p

julia> @p PAndQ.only_field(p ∧ q)
2-element Vector{Tree{typeof(identity), Atom{Symbol}}}:
 p
 q
```
"""
only_field(p::Atom) = p.statement
only_field(p::Literal) = p.atom
only_field(p::Tree) = p.nodes
only_field(p::Clause) = p.literals
only_field(p::Normal) = p.clauses

"""
    operator_to_proposition(::Union{LogicalOperator, Proposition})

Construct a [`Proposition`](@ref) with the given operator.

# Examples
```jldoctest
julia> PAndQ.operator_to_proposition(⊤)
⊤

julia> PAndQ.operator_to_proposition(¬)
¬_

julia> PAndQ.operator_to_proposition(∧)
_ ∧ __
```
"""
operator_to_proposition(no::NullaryOperator) = Clause(no)
operator_to_proposition(uo::UnaryOperator) = uo(Atom(:_))
operator_to_proposition(bo::BinaryOperator) = bo(Atom(:_), Atom(:__))
operator_to_proposition(p::Proposition) = p

"""
    atomize(x)

If `x` is a symbol, return an expression that
instantiates it as an [`Atom`](@ref) if it is undefined.
If `x` is an expression, traverse it with recursive calls to `atomize`
(ignoring variable assignment, keyword arguments, and anonymous functions).
Otherise, return x.
"""
atomize(x::Symbol) = :((@isdefined $x) ? $x : $(Atom(x)))
atomize(x::Expr) = length(x.args) == 0 ? x : Expr(x.head,
    Meta.isexpr(x, (:(=), :kw, :->)) ? x.args[1] : atomize(x.args[1]),
    map(atomize, x.args[2:end])...
)
atomize(x) = x

# Macros

"""
    @atoms(ps...)

Instantiate and define [`Atom`](@ref)s with symbols and return a vector containing them.

!!! info
    Atoms are defined in the global scope as constants.

Examples
```jldoctest
julia> @atoms p q
2-element Vector{Atom{Symbol}}:
 p
 q

julia> p
p

julia> q
q
```
"""
macro atoms(ps...)
    esc(quote
        $(map(p -> :(const $p = $(Atom(p))), ps)...)
        Atom{Symbol}[$(ps...)]
    end)
end
#=
Source:
Symbolics.jl
https://github.com/JuliaSymbolics/Symbolics.jl
=#

"""
    @p(expression)

Instantiates each undefined variable
(ignoring variable assignment and keyword arguments)
as an [`Atom{Symbol}`](@ref).

# Examples
```jldoctest
julia> @p x = p ∧ q
p ∧ q

julia> @p x → r
(p ∧ q) → r
```
"""
macro p(expression)
    esc(:($(atomize(expression))))
end

"""
    @p_str(x)

# Examples
```jldoctest
julia> p = @p_str("x")
x

julia> p"p ∧ q, Clause(and)"
(Tree(and, Tree(identity, Atom(:x)), Tree(identity, Atom(:q))), Clause(and, []))
```
"""
macro p_str(p)
    esc(:(@p $(Meta.parse(p))))
end

# Utility

_atoms(p::Literal{typeof(identity)}) = p.atom
_atoms(p::Tree{typeof(identity)}) = _atoms(only(p.nodes))
_atoms(p) = p

"""
    atoms(::Proposition)

Returns an iterator of [`Atom`](@ref)s
contained in the given [`Proposition`](@ref).

!!! warning
    Some atoms may optimized out of an expression, such as in `p ∧ ⊥ == ⊥`.

# Examples
```jldoctest
julia> @p collect(atoms(¬p))
1-element Vector{Atom{Symbol}}:
 p

julia> @p collect(atoms(p ∧ q))
2-element Vector{Atom{Symbol}}:
 p
 q
```
"""
atoms(p) = Iterators.filter(leaf -> leaf isa Atom, Iterators.map(_atoms, Leaves(p)))
