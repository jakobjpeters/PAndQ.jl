
import AbstractTrees: children, nodevalue, printnode
using Base.Meta: isexpr, parse
using AbstractTrees: childtype, Leaves, PreOrderDFS

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
    Use [`@atoms`](@ref) and [`@atomize`](@ref) as shortcuts to
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
julia> @atomize r = ¬p
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
julia> @atomize r = p ⊻ q
p ⊻ q

julia> @atomize ¬r → s
(p ↔ q) → s
```
"""
struct Tree{LO <: LogicalOperator, P <: Proposition} <: Expressive{LO}
    nodes::Vector{P}

    Tree(::NO) where NO <: NullaryOperator = new{NO, Tree}([])
    Tree(::UO, node::A) where {UO <: UnaryOperator, A <: Atom} = new{UO, A}([node])
    function Tree(lo::LO, nodes::Tree...) where LO <: LogicalOperator
        _arity, _length = arity(lo), length(nodes)
        _arity != _length &&
            error("`arity($lo) == $_arity`, but `$_length` arguments were given")
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

julia> @atomize Clause(p)
p

julia> @atomize Clause(or, [¬p, q])
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
julia> @atomize s = Normal(and, [Clause(or, [p, q]), Clause(or, ¬r)])
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

Return an iterator over the child nodes of the given [`Proposition`](@ref).

# Examples
```jldoctest
julia> @atomize PAndQ.children(p)
()

julia> @atomize PAndQ.children(¬p)
(Atom(:p),)

julia> @atomize PAndQ.children(p ∧ q)
2-element Vector{Tree{typeof(identity), Atom{Symbol}}}:
 p
 q
```
"""
children(p::Atom) = ()
children(p::Literal) = (p.atom,)
children(p::Compound) = only_field(p)

"""
    nodevalue(::Compound)

# Examples
```jldoctest
julia> @atomize PAndQ.nodevalue(¬p)
not (generic function with 19 methods)

julia> @atomize PAndQ.nodevalue(p ∧ q)
and (generic function with 23 methods)
```
"""
nodevalue(::Compound{LO}) where LO = LO.instance

"""
    printnode(::IO, ::Proposition; kwargs...)

!!! note
    Instances of [`Compound{typeof(identity)}`](@ref Compound) are represented as `I`.

See also [`Proposition`](@ref).

# Examples
```jldoctest
julia> @atomize PAndQ.printnode(stdout, p)
p
julia> @atomize PAndQ.printnode(stdout, ¬p)
¬
julia> @atomize PAndQ.printnode(stdout, p ∧ q)
∧
```
"""
printnode(io::IO, p::Atom; kwargs...) = show(io, MIME"text/plain"(), p)
printnode(io::IO, ::Compound{typeof(identity)}; kwargs...) = print(io, "I")
printnode(io::IO, p::Compound; kwargs...) = print(io, operator_to_symbol(nodevalue(p)))

## Utilities

"""
    union_all_type(::Proposition)

Return the `UnionAll` type of a [`Proposition`](@ref).

# Examples
```jldoctest
julia> @atomize PAndQ.union_all_type(p)
Atom

julia> @atomize PAndQ.union_all_type(p ∧ q)
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
julia> @atomize PAndQ.only_field(p)
:p

julia> @atomize PAndQ.only_field(p ∧ q)
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

If `x` is a symbol, return an expression that instantiates it as an
[`Atom`](@ref) if it is undefined in the caller's scope.
If `x` is an expression, traverse it with recursive calls to `atomize`
Otherise, return x.
"""
atomize(x::Symbol) = :((@isdefined $x) ? $x : $(Atom(x)))
function atomize(x::Expr)
    if length(x.args) == 0 x
    elseif isexpr(x, :$); :(Atom($(only(x.args))))
    elseif isexpr(x, (:kw, :<:))
        Expr(x.head, x.args[1], atomize(x.args[2]))
    elseif isexpr(x, (:struct, :where)) x # TODO
    else Expr(x.head, # TODO
        isexpr(x, (:(=), :->, :function)) ? x.args[1] : atomize(x.args[1]),
        map(atomize, x.args[2:end])...
    )
    end
end
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
    @atomize(expression)

Instantiate undefined variables and interpolated values inline as [`Atom`](@ref)s.

!!! warning
    This macro attempts to ignore symbols that are being assigned a value.
    For example, `@atomize f(; x = p) = x ∧ q` should be equivalent to
    `@atomize f(; x = Atom(:p)) = x ∧ Atom(:q)`.
    However, this feature is in-progress and only works in some cases.
    The implementation is cautious to skip the parts
    of the expression that it cannot yet handle.

# Examples
```jldoctest
julia> @atomize x = p ∧ q
p ∧ q

julia> @atomize x → r
(p ∧ q) → r

julia> @atomize \$1 ∧ \$(1 + 1)
Atom(1) ∧ Atom(2)
```
"""
macro atomize(expression)
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
    esc(:(@atomize $(parse(p))))
end

# Utility

"""
    atoms(p)

Returns an iterator of [`Atom`](@ref)s contained in `p`.

# Examples
```jldoctest
julia> @atomize collect(atoms(¬p))
1-element Vector{Atom{Symbol}}:
 p

julia> @atomize collect(atoms(p ∧ q))
2-element Vector{Atom{Symbol}}:
 p
 q
```
"""
atoms(p) = Iterators.filter(leaf -> leaf isa Atom, Leaves(p))

"""
    operators(p)

Returns an iterator of [`LogicalOperator`](@ref)s contained in `p`.

# Examples
```jldoctest
julia> @atomize collect(operators(¬p))
1-element Vector{typeof(not)}:
 not (generic function with 19 methods)

julia> @atomize collect(operators(¬p ∧ q))
3-element Vector{Function}:
 and (generic function with 23 methods)
 not (generic function with 19 methods)
 identity (generic function with 1 method)
```
"""
operators(p) = Iterators.map(nodevalue, Iterators.filter(node -> !isa(node, Atom), PreOrderDFS(p)))
