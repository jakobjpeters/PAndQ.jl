
import Base: map
import AbstractTrees: children, nodevalue, printnode
using Base.Meta: isexpr, parse
using AbstractTrees: childtype, Leaves, nodevalues, PreOrderDFS

# Abstract Types

"""
    Proposition

The set of [well-formed logical formulae](https://en.wikipedia.org/wiki/Well-formed_formula).

Supertype of [`Atom`](@ref) and [`Compound`](@ref).
"""
abstract type Proposition end

"""
    Atom <: Proposition

A proposition with [no deeper propositional structure](https://en.wikipedia.org/wiki/Atomic_formula).

Subtype of [`Proposition`](@ref).
Supertype of [`Constant`](@ref) and [`Variable`](@ref).
"""
abstract type Atom <: Proposition end

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
    Constant{T} <: Atom
    Constant(::T)

An [atomic sentence](https://en.wikipedia.org/wiki/Atomic_sentence).

!!! tip
    Define pretty-printing for an instance of `Constant{T}` by overloading
    [`show(io::IO, ::MIME"text/plain", p::Constant{T})`](@ref show).

!!! tip
    Use [`@atoms`](@ref) and [`@atomize`](@ref) as shortcuts to
    define constants or instantiate them inline, respectively.

Subtype of [`Atom`](@ref).

# Examples
```jldoctest
julia> Constant(1)
\$(1)

julia> Constant("Logic is fun")
\$("Logic is fun")
```
"""
struct Constant{T} <: Atom
    value::T
end

"""
    Variable <: Atom

An [atomic formula](https://en.wikipedia.org/wiki/Atomic_formula).

!!! tip
    Use [`@atoms`](@ref) and [`@atomize`](@ref) as shortcuts to
    define variables or instantiate them inline, respectively.

Subtype of [`Atom`](@ref).

# Examples
```jldoctest
julia> Variable(:p)
p

julia> Variable(:q)
q
```
"""
struct Variable <: Atom
    symbol::Symbol

    function Variable(symbol)
        s = string(symbol)
        isempty(s) && error("The symbol must be non-empty")
        all(c -> isprint(c) && !isspace(c), s) ||
            error("The symbol must contain only printable non-space characters")
        new(symbol)
    end
end

"""
    Literal{UO <: UnaryOperator, A <: Atom} <: Compound{UO}
    Literal(::UO, ::A)
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
struct Literal{UO <: UnaryOperator, A <: Atom} <: Compound{UO}
    atom::A

    Literal(::UO, atom::A) where {UO <: UnaryOperator, A <: Atom} = new{UO, A}(atom)
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
        new{LO, eltype(nodes)}(collect(nodes))
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
(Variable(:p),)

julia> @atomize PAndQ.children(p ∧ q)
2-element Vector{Tree{typeof(identity), Variable}}:
 p
 q
```
"""
children(p::Atom) = ()
children(p::Literal) = (p.atom,)
children(p::Union{Tree, Clause, Normal}) = only_field(p)

"""
    nodevalue(::Compound)

# Examples
```jldoctest
julia> @atomize PAndQ.nodevalue(¬p)
not (generic function with 19 methods)

julia> @atomize PAndQ.nodevalue(p ∧ q)
and (generic function with 25 methods)
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

_map(P::Type{<:Tree{LO}}, children) where LO = P(LO.instance, children...)
_map(P::Type{<:Union{Clause, Normal}}, lo, children) = P(lo, children)

"""
    map(f, ::Proposition)

Apply `f` to each [`Atom`](@ref) in the [`Proposition`](@ref).

# Examples
```jldoctest
julia> @atomize map(Tree ∘ ¬, p ∧ q)
¬p ∧ ¬q

julia> @atomize map(p ∧ q) do atom
           println(atom)
           atom
       end
Variable(:p)
Variable(:q)
p ∧ q
```
"""
map(f, p::Atom) = f(p)
map(f, p::Tree{LO}) where LO =
    union_all_type(p)(LO.instance, map(child -> map(f, child), children(p))...)
map(f, p::Union{Clause{AO}, Normal{AO}}) where AO =
    union_all_type(p)(AO.instance, map(child -> map(f, child), children(p)))

"""
    union_all_type(::Proposition)

Return the `UnionAll` type of a [`Proposition`](@ref).

# Examples
```jldoctest
julia> @atomize PAndQ.union_all_type(p)
Variable

julia> @atomize PAndQ.union_all_type(p ∧ q)
Tree
```
"""
function union_all_type end
for T in (:Constant, :Variable, :Literal, :Tree, :Clause, :Normal)
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
2-element Vector{Tree{typeof(identity), Variable}}:
 p
 q
```
"""
only_field(p::Constant) = p.value
only_field(p::Variable) = p.symbol
only_field(p::Literal) = p.atom
only_field(p::Tree) = p.nodes
only_field(p::Clause) = p.literals
only_field(p::Normal) = p.clauses

"""
    symbol_value
"""
symbol_value(x::Symbol) = x, :($(Variable(x)))
symbol_value(x) = isexpr(x, :(=)) ?
    (first(x.args), :(Constant($(last(x.args))))) :
    error("syntax should be `symbol = value` or `symbol`")

"""
    atomize(x)

If `x` is a symbol, return an expression that instantiates it as a
[`Variable`](@ref) if it is undefined in the caller's scope.
If `isexpr(x, :\$)`, return an expression that instantiates it as a [`Constant`](@ref).
If `x` is another expression, traverse it with recursive calls to `atomize`
Otherise, return x.
"""
atomize(x::Symbol) = :((@isdefined $x) ? $x : $(Variable(x)))
function atomize(x::Expr)
    if length(x.args) == 0 x
    elseif isexpr(x, :$); :(Constant($(only(x.args))))
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
    @atoms(xs...)

Instantiate and define [`Atom`](@ref)s as `const` and return a vector containing them.

Expressions of the form `symbol = value` and `symbol` are defined as
`const symbol = Constant(value)` and `const symbol = Variable(:symbol)`, respectively.

See also [`Atom`](@ref) and [`Variable`](@ref).

Examples
```jldoctest
julia> @atoms a = 1 p
2-element Vector{Atom}:
 \$(1)
 p

julia> a
\$(1)

julia> p
p
```
"""
macro atoms(xs...)
    symbols_values = map(symbol_value, xs)
    esc(quote
        $(map(((symbol, value),) -> :(const $symbol = $value), symbols_values)...)
        [$(map(first, symbols_values)...)]
    end)
end

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
\$(1) ∧ \$(2)
```
"""
macro atomize(expression)
    esc(:($(atomize(expression))))
end

"""
    @p_str(x)

# Examples
```jldoctest
julia> x = @p_str("p")
p

julia> p"[x ∧ q, Clause(and)]"
2-element Vector{Compound{typeof(and)}}:
 p ∧ q
 ⊤
```
"""
macro p_str(p)
    esc(:(@atomize $(parse(p))))
end

# Utility

"""
    atoms(p, T = Atom)

Return an iterator of each [`Atom`](@ref) of type `T` contained in `p`.

See also [`Constant`](@ref) and [`Variable`](@ref).

# Examples
```jldoctest
julia> @atomize collect(atoms(p ∧ q))
2-element Vector{Variable}:
 p
 q

julia> @atomize collect(atoms(p ∧ q ∨ \$1 ∧ \$2, Constant))
2-element Vector{Constant{Int64}}:
 \$(1)
 \$(2)
```
"""
atoms(p, T = Atom) = Iterators.filter(leaf -> leaf isa T, Leaves(p))

"""
    operators(p)

Return an iterator of each [`LogicalOperator`](@ref) contained in `p`.

# Examples
```jldoctest
julia> @atomize collect(operators(¬p))
1-element Vector{typeof(not)}:
 not (generic function with 19 methods)

julia> @atomize collect(operators(¬p ∧ q))
3-element Vector{Function}:
 and (generic function with 25 methods)
 not (generic function with 19 methods)
 identity (generic function with 1 method)
```
"""
operators(p) = Iterators.filter(
    node -> !isa(node, Atom),
    nodevalues(PreOrderDFS(p))
)
