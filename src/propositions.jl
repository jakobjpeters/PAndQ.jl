
import Base: map
import AbstractTrees: children, nodevalue, printnode
using Base.Meta: isexpr, parse
using AbstractTrees: childtype, Leaves, nodevalues, PreOrderDFS

# Internals

## Types

### Abstract

"""
    Proposition

A logical [proposition](https://en.wikipedia.org/wiki/Proposition).

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

A proposition composed from connecting atomic propositions with logical operators.

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

# Atoms

"""
    Constant{T} <: Atom
    Constant(value::T)

An [atomic sentence](https://en.wikipedia.org/wiki/Atomic_sentence).

Subtype of [`Atom`](@ref).

# Examples
```jldoctest
julia> PAndQ.Constant(1)
\$(1)

julia> PAndQ.Constant("Logic is fun")
\$("Logic is fun")
```
"""
struct Constant{T} <: Atom
    value::T
end

"""
    Variable <: Atom

An [atomic formula](https://en.wikipedia.org/wiki/Atomic_formula).

Subtype of [`Atom`](@ref).

# Examples
```jldoctest
julia> PAndQ.Variable(:p)
p

julia> PAndQ.Variable(:q)
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

# Internal

## Types

### Concrete

"""
    Literal{UO <: UnaryOperator, A <: Atom} <: Compound{UO}
    Literal(::UO, ::A)
    Literal(::Union{Atom, Literal})

A proposition represented by [an atomic formula or its negation]
(https://en.wikipedia.org/wiki/Literal_(mathematical_logic)).

Subtype of [`Compound`](@ref).
See also [`UnaryOperator`](@ref) and [`Atom`](@ref).

# Examples
```jldoctest
julia> @atomize PAndQ.Literal(𝒾, p)
p

julia> @atomize PAndQ.Literal(¬, p)
¬p
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
julia> PAndQ.Tree(⊤)
⊤

julia> @atomize PAndQ.Tree(¬, p)
¬p

julia> @atomize PAndQ.Tree(and, PAndQ.Tree(p), PAndQ.Tree(q))
p ∧ q
```
"""
struct Tree{LO <: LogicalOperator, P <: Proposition} <: Expressive{LO}
    nodes::Vector{P}

    Tree(::NO) where NO <: NullaryOperator = new{NO, Tree}([])
    Tree(::UO, node::A) where {UO <: UnaryOperator, A <: Atom} = new{UO, A}([node])
    function Tree(lo::LO, nodes::Tree...) where LO <: LogicalOperator
        _arity, _length = arity(lo), length(nodes)
        _arity != _length &&
            error("`arity($lo) == $_arity`, but `$_length` argument$(_length == 1 ? " was" : "s were") given")
        new{LO, eltype(nodes)}(collect(nodes))
    end
end

"""
    Clause{AO <: AndOr, L <: Literal} <: Compound{AO}
    Clause(::AO, ps = Literal[])
    Clause(::AO, p::Proposition)
    Clause(::Union{NullaryOperator, Atom, Literal})

A proposition represented as either a [conjunction or disjunction of literals]
(https://en.wikipedia.org/wiki/Clause_(logic)).

!!! info
    An empty `Clause` is [logically equivalent](@ref ==) to the
    neutral element of it's binary operator.

Subtype of [`Compound`](@ref).
See also [`AndOr`](@ref), [`Literal`](@ref), and [`NullaryOperator`](@ref).

# Examples
```jldoctest
julia> PAndQ.Clause(∧)
⊤

julia> @atomize PAndQ.Clause(p)
p

julia> @atomize PAndQ.Clause(∨, [¬p, q])
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
    An empty `Normal` is [logically equivalent](@ref ==) to the
    neutral element of it's binary operator.

Subtype of [`Expressive`](@ref).
See also [`AndOr`](@ref), [`Clause`](@ref),
[`NullaryOperator`](@ref), and [`Proposition`](@ref).

# Examples
```jldoctest
julia> PAndQ.Normal(⊤)
⊤

julia> @atomize PAndQ.Normal(∧, p ⊻ q)
(p ∨ q) ∧ (¬p ∨ ¬q)
```
"""
struct Normal{AO <: AndOr, C <: Clause} <: Expressive{AO}
    clauses::Vector{C}

    Normal(::A, clauses::Vector{C} = Clause{typeof(or)}[]) where {A <: typeof(and), C <: Clause{typeof(or)}} =
        new{A, C}(union(clauses))
    Normal(::O, clauses::Vector{C} = Clause{typeof(and)}[]) where {O <: typeof(or), C <: Clause{typeof(and)}} =
        new{O, C}(union(clauses))
end

## AbstractTrees.jl

"""
    children(::Proposition)

Return an iterator over the child nodes of the given [`Proposition`](@ref).

# Examples
```jldoctest
julia> @atomize PAndQ.children(p)
()

julia> @atomize PAndQ.children(¬p)
(PAndQ.Variable(:p),)

julia> @atomize PAndQ.children(p ∧ q)
2-element Vector{PAndQ.Tree{typeof(identity), PAndQ.Variable}}:
 p
 q
```
"""
children(p::Atom) = ()
children(p::Literal) = (p.atom,)
children(p::Tree) = p.nodes
children(p::Clause) = p.literals
children(p::Normal) = p.clauses

"""
    nodevalue(::Compound)

See also [`Compound`](@ref).

# Examples
```jldoctest
julia> @atomize PAndQ.nodevalue(¬p)
not (generic function with 19 methods)

julia> @atomize PAndQ.nodevalue(p ∧ q)
and (generic function with 18 methods)
```
"""
nodevalue(::Compound{LO}) where LO = LO.instance

"""
    printnode(::IO, ::Proposition; kwargs...)

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
printnode(io::IO, p::Union{Literal, Tree}; kwargs...) = print(io, symbol_of(nodevalue(p)))
printnode(io::IO, p::Union{Clause, Normal}; kwargs...) =
    print(io, symbol_of((isempty(children(p)) ? only ∘ left_neutrals : 𝒾)(nodevalue(p))))

## Utilities

"""
    child(x)

Equivalent to `only(children(x))`

See also [`children`](@ref)
"""
const child = only ∘ children

for T in (:Constant, :Variable, :Literal, :Tree, :Clause, :Normal)
    @eval union_all_type(::$T) = $T
end

"""
    union_all_type(::Proposition)

Return the `UnionAll` type of a [`Proposition`](@ref).

# Examples
```jldoctest
julia> @atomize PAndQ.union_all_type(p)
PAndQ.Variable

julia> @atomize PAndQ.union_all_type(p ∧ q)
PAndQ.Tree
```
"""
union_all_type

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
    elseif isexpr(x, :$);
        value = only(x.args)
        :(
            if (@isdefined PAndQ) PAndQ.Constant($value)
            elseif (@isdefined Constant) Constant($value)
            else error("Either `PAndQ` or `PAndQ.Constant` must be loaded")
            end
        )
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

"""
    symbol_value
"""
symbol_value(x::Symbol) = x, x
symbol_value(x) = isexpr(x, :(=)) ?
    (first(x.args), last(x.args)) :
    error("Each atom must have the syntax `symbol` or `symbol = value`")

# Macros

"""
    @atomize(expression)

Instantiate undefined symbols as [`Variable`](@ref)s
and interpolated values as [`Constant`](@ref)s inline.

!!! warning
    This macro attempts to ignore symbols that are being assigned a value.
    For example, `@atomize f(; x = p) = x ∧ q` should be equivalent to
    `@atomize f(; x = PAndQ.Atom(:p)) = x ∧ PAndQ.Atom(:q)`.
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
    @atoms(xs...)

Define atoms and return a vector containing them.

Expressions of the form `symbol` and `symbol = value` are defined as
`@atomize symbol = symbol` and `@atomize symbol = value`, respectively.

See also [`@atomize`](@ref).

Examples
```jldoctest
julia> @atoms p q = ¬\$1
2-element Vector{PAndQ.Proposition}:
 p
 ¬\$(1)

julia> p
p

julia> q
¬\$(1)
```
"""
macro atoms(xs...)
    symbols_values = map(symbol_value, xs)
    esc(quote
        $(map(((symbol, value),) -> :($symbol = $(atomize(value))), symbols_values)...)
        [$(map(first, symbols_values)...)]
    end)
end

# Utility

"""
    atoms(p, T = Atom)

Return an iterator of each [`Atom`](@ref) of type `T` contained in `p`.

# Examples
```jldoctest
julia> @atomize collect(atoms(p ∧ q))
2-element Vector{PAndQ.Variable}:
 p
 q

julia> @atomize collect(atoms(p ∧ q ∨ \$1 ∧ \$2, PAndQ.Constant))
2-element Vector{PAndQ.Constant{Int64}}:
 \$(1)
 \$(2)
```
"""
atoms(p, T = Atom) = Iterators.filter(leaf -> leaf isa T, Leaves(p))

"""
    operators(::Proposition)

Return an iterator of each logical [operator](@ref operators_operators)
contained in the given [`Proposition`](@ref).

# Examples
```jldoctest
julia> @atomize collect(operators(¬p))
1-element Vector{typeof(not)}:
 not (generic function with 19 methods)

julia> @atomize collect(operators(¬p ∧ q))
3-element Vector{Function}:
 and (generic function with 18 methods)
 not (generic function with 19 methods)
 identity (generic function with 1 method)
```
"""
operators(p) = Iterators.filter(
    node -> !isa(node, Atom),
    nodevalues(PreOrderDFS(p))
)

"""
    map(f, ::Proposition)

Apply `f` to each [`Atom`](@ref) in the given [`Proposition`](@ref).

# Examples
```jldoctest
julia> @atomize map(PAndQ.Tree ∘ ¬, p ∧ q)
¬p ∧ ¬q

julia> @atomize map(p ∧ q) do atom
           println(atom)
           atom
       end
PAndQ.Variable(:p)
PAndQ.Variable(:q)
p ∧ q
```
"""
map(f, p::Atom) = f(p)
map(f, p::Literal) = Literal(nodevalue(p), f(child(p)))
map(f, p::Tree) =
    union_all_type(p)(nodevalue(p), map(child -> map(f, child), children(p))...)
map(f, p::Union{Clause, Normal}) =
    union_all_type(p)(nodevalue(p), map(child -> map(f, child), children(p)))
