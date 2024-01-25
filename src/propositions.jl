
import AbstractTrees: HasNodeType, NodeType, children, nodetype, nodevalue, printnode
import Base: map
using AbstractTrees: Leaves, PreOrderDFS, childtype, nodevalues
using Base.Iterators: Stateful, flatmap
using Base.Meta: isexpr, parse
using ReplMaker: complete_julia, initrepl
using .PicoSAT: PicoSAT, initialize, picosat_print, picosat_reset

## Types

### Abstract

"""
    Proposition

A [proposition](https://en.wikipedia.org/wiki/Proposition).

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
    Compound <: Proposition

A proposition composed from connecting [`Atom`](@ref)s
with one or more [`Operator`](@ref)s.

Subtype of [`Proposition`](@ref).
Supertype of [`Tree`](@ref), [`Clause`](@ref), and [`Normal`](@ref).
"""
abstract type Compound <: Proposition end

### Concrete

"""
    Constant{T} <: Atom
    Constant(::T)

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
        isempty(s) && throw(ArgumentError("The symbol must be non-empty"))
        all(c -> isprint(c) && !isspace(c), s) ||
            throw(ArgumentError("The symbol must contain only printable non-space characters"))
        new(symbol)
    end
end

"""
    Tree{O <: Operator, AT <: Union{Atom, Tree}, N} <: Compound
    Tree(::NullaryOperator, ::Atom)
    Tree(::Operator, ::Tree...)
    Tree(::Proposition)

A [`Proposition`](@ref) represented by an [abstract syntax tree]
(https://en.wikipedia.org/wiki/Abstract_syntax_tree).

Subtype of [`Compound`](@ref).
See also [`Atom`](@ref), [`NullaryOperator`](@ref), and [`Operator`](@ref).

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
struct Tree{O <: Operator, P <: Proposition, N} <: Compound
    nodes::NTuple{N, P}

    function Tree(::O, nodes...) where O <: Operator
        _arity, _length = arity(O.instance), length(nodes)
        _arity != _length && throw(ArgumentError(
            "`arity($(O.instance)) == $_arity`, but `$_length` argument$(_length == 1 ? " was" : "s were") given"
        ))
        new{O, eltype(nodes), _arity}(nodes)
    end
end

"""
    Literal <: Tree{<:Operator, <:Atom, 1}

A proposition represented by [an atomic formula or its negation]
(https://en.wikipedia.org/wiki/Literal_(mathematical_logic)).

See also [`Operator`](@ref), [`Atom`](@ref), and [`Tree`](@ref).

# Examples
```jldoctest
julia> @atomize PAndQ.Literal(𝒾, p)
p

julia> @atomize PAndQ.Literal(¬, p)
¬p
```
"""
const Literal = Tree{<:Operator, <:Atom, 1}

"""
    Clause{AO <: AndOr, A <: AbstractVector{<:Atom}, L <: AbstractVector{Int}} <: Compound
    Clause(::AO, ::A, ::L)

A proposition represented as either a [conjunction or disjunction of literals]
(https://en.wikipedia.org/wiki/Clause_(logic)).

!!! info
    An empty `Clause` is [logically equivalent](@ref ==) to the
    neutral element of it's binary operator.

Subtype of [`Compound`](@ref).
See also [`Atom`](@ref), [`Literal`](@ref), [`AndOr`](@ref), and [`NullaryOperator`](@ref).

# Examples
```jldoctest
julia> PAndQ.Clause(∧, PAndQ.Atom[], Set{Int}())
⊤

julia> @atomize PAndQ.Clause(∧, [p], Set(1))
p

julia> @atomize PAndQ.Clause(∨, [p, q], Set((1, -2)))
¬q ∨ p
```
"""
struct Clause{AO <: AndOr, A <: Atom} <: Compound
    atoms::Vector{A}
    literals::Set{Int}

    Clause(::AO, atoms::Vector{A}, literals) where {AO <: AndOr, A <: Atom} =
        new{AO, A}(atoms, literals)
end

"""
    Normal{AO <: AndOr, A <: Atom, C <: AbstractVector{<:AbstractVector{Int}}} <: Compound
    Normal(::AO, ::A, ::C)

A [`Proposition`](@ref) represented in [conjunctive]
(https://en.wikipedia.org/wiki/Conjunctive_normal_form) or [disjunctive]
(https://en.wikipedia.org/wiki/Disjunctive_normal_form) normal form.

!!! info
    An empty `Normal` is [logically equivalent](@ref ==) to the
    neutral element of it's binary operator.

Subtype of [`Compound`](@ref).
See also [`Clause`](@ref), [`AndOr`](@ref), and [`NullaryOperator`](@ref).

# Examples
```jldoctest
julia> PAndQ.Normal(∧, PAndQ.Atom[], Set{Set{Int}}())
⊤

julia> @atomize PAndQ.Normal(∧, [p, q], Set(map(Set, ((1, 2), (-1, -2)))))
(¬p ∨ ¬q) ∧ (p ∨ q)
```
"""
struct Normal{AO <: AndOr, A <: Atom} <: Compound
    atoms::Vector{A}
    clauses::Set{Set{Int}}

    Normal(::AO, atoms::Vector{A}, clauses) where {AO <: AndOr, A <: Atom} =
        new{AO, A}(atoms, clauses)
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
(PAndQ.Variable(:p), PAndQ.Variable(:q))
```
"""
children(p::Tree) = p.nodes
children(p::Clause) = Iterators.map(i -> Literal(signbit(i) ? (¬) : 𝒾, p.atoms[abs(i)]), p.literals)
function children(p::Normal)
    and_or = dual(nodevalue(p))
    Iterators.map(p.clauses) do clause
        atoms = p.atoms[collect(Iterators.map(abs, clause))]
        Clause(and_or, atoms, Set(Iterators.map(literal -> sign(literal) * findfirst(==(p.atoms[abs(literal)]), atoms), clause)))
    end
end

"""
    nodevalue(::Union{Tree{O}, Clause{O}, Normal{O}}) where O

Return `O.instance`.

See also [`Compound`](@ref).

# Examples
```jldoctest
julia> @atomize PAndQ.nodevalue(¬p)
¬

julia> @atomize PAndQ.nodevalue(p ∧ q)
∧
```
"""
nodevalue(::Union{Tree{O}, Clause{O}, Normal{O}}) where O = O.instance

_printnode(p::Union{NullaryOperator, Atom}) = p
_printnode(p::Tree) = nodevalue(p)
_printnode(p::Union{Clause, Normal}) =
    (isempty(children(p)) ? something ∘ initial_value : 𝒾)(nodevalue(p))

"""
    printnode(::IO, ::Union{NullaryOperator, Proposition}; kwargs...)

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
printnode(io::IO, p::Union{NullaryOperator, Proposition}; kwargs...) =
    show(io, MIME"text/plain"(), _printnode(p))

"""
    NodeType(::Type{<:Atom})

See also [`Atom`](@ref).
"""
NodeType(::Type{<:Atom}) = HasNodeType()

"""
    nodetype(::Type{<:Atom})

See also [`Atom`](@ref).
"""
nodetype(::Type{A}) where A <: Atom = A

## Utilities

"""
    child(x)

Equivalent to `only ∘ children`

See also [`children`](@ref).
"""
const child = only ∘ children

"""
    atomize(x)

If `x` is a symbol, return an expression that instantiates it as a
[`Variable`](@ref) if it is undefined in the caller's scope.
If `isexpr(x, :\$)`, return an expression that instantiates it as a [`Constant`](@ref).
If `x` is a different expression, traverse it with recursive calls to `atomize`.
Otherise, return x.
"""
atomize(x::Symbol) = :((@isdefined $x) ? $x : $(Variable(x)))
function atomize(x::Expr)
    if length(x.args) == 0 x
    elseif isexpr(x, :$)
        value = only(x.args)
        :(
            if @isdefined PAndQ; PAndQ.Constant($value)
            elseif @isdefined Constant; Constant($value)
            else error("either `PAndQ` or `PAndQ.Constant` must be loaded")
            end
        )
    elseif isexpr(x, (:kw, :<:)) Expr(x.head, x.args[1], atomize(x.args[2]))
    elseif isexpr(x, (:struct, :where)) x # TODO
    else Expr(x.head, # TODO
        (isexpr(x, (:(=), :->, :function)) ? 𝒾 : atomize)(x.args[1]),
        map(atomize, x.args[2:end])...
    )
    end
end
atomize(x) = x

__negated_normal(p::Tree{<:NullaryOperator}) = Some(nodevalue(p))
__negated_normal(p) = p

_negated_normal(p::Some) = Tree(something(p))
_negated_normal(p::Tree{typeof(¬), <:Tree{typeof(¬)}}) = Tree(child(child(p)))
_negated_normal(p) = p

__negated_normal!(output_stack, operator, nodes...) =
    push!(output_stack, _negated_normal(Tree(something(operator(map(__negated_normal, nodes)...)))))

_negated_normal!(operator_stack, i, p::Tree{typeof(𝒾)}) = operators
_negated_normal!(operator_stack, i, p) = push!(operator_stack, i => nodevalue(p))

negated_normal!(operator_stack, input_stack, output_stack, operator::UnaryOperator) =
    operator_stack, input_stack, __negated_normal!(output_stack, operator, pop!(output_stack))
negated_normal!(operator_stack, input_stack, output_stack, operator::AndOr) =
    operator_stack, input_stack, __negated_normal!(output_stack, operator, pop!(output_stack), pop!(output_stack))
negated_normal!(operator_stack, input_stack, output_stack, p::Union{Tree{<:NullaryOperator}, Literal}) =
    operator_stack, input_stack, push!(output_stack, p)
negated_normal!(operator_stack, input_stack, output_stack, p::Tree{typeof(¬), <:Tree{typeof(¬)}}) =
    operator_stack, push!(input_stack, Tree(child(child(p)))), output_stack
function negated_normal!(operators, input_stack, output_stack, p::Tree{typeof(¬)})
    _child = child(p)
    operators, push!(input_stack, Tree(dual(nodevalue(_child))(map(¬, _child.nodes)...))), output_stack
end
negated_normal!(operator_stack, input_stack, output_stack, p::Tree{<:Union{typeof(𝒾), AndOr}}) =
    _negated_normal!(operator_stack, length(output_stack), p), append!(input_stack, p.nodes), output_stack

"""
    negated_normal(p)
"""
function negated_normal(p)
    operator_stack, input_stack, output_stack = Pair{Int, Operator}[], Tree[p], Tree[]

    while !all(isempty, (operator_stack, input_stack))
        i_operator = isempty(operator_stack) ? nothing : last(operator_stack)
        negated_normal!(operator_stack, input_stack, output_stack,
            isnothing(i_operator) || first(i_operator) + arity(last(i_operator)) != length(output_stack) ?
                pop!(input_stack) :
                last(pop!(operator_stack)))
    end

    only(output_stack)
end

___distribute(p::Tree{typeof(∧)}, q) = distribute(p ∨ q)
___distribute(p, q) = p ∨ q

__distribute(p::Tree{typeof(∧)}, q) = distribute(p ∨ q)
__distribute(p, q) = ___distribute(distribute(q), p)

_distribute(p::Literal, q::Literal) = p ∨ q
function _distribute(p::Tree{typeof(∧)}, q)
    (r, s), t = p.nodes, distribute(q)
    distribute(distribute(r) ∨ t) ∧ distribute(distribute(s) ∨ t)
end
_distribute(p::Tree{typeof(∨)}, q) = __distribute(distribute(p), q)
_distribute(p, q) = distribute(q ∨ p)

"""
    distribute(p)
"""
distribute(p::Union{NullaryOperator, Atom, Literal}) = p
distribute(p::Tree{<:NullaryOperator}) = nodevalue(p)
distribute(p::Tree{typeof(∧)}) = ∧(map(distribute, p.nodes)...)
distribute(p::Tree{typeof(∨)}) = _distribute(p.nodes...)

_flatten!(literals, qs, ::Union{typeof(⊥), Tree{typeof(⊥)}}) = literals, qs
_flatten!(literals, qs, p::Literal) = push!(literals, p), qs
_flatten!(literals, qs, p::Tree{typeof(∨)}) = literals, append!(qs, p.nodes)
_flatten!(literals, qs, p) = literals, qs, p

flatten!(mapping, clauses, qs, ::Union{typeof(⊤), Tree{typeof(⊤)}}) = mapping, clauses, qs
function flatten!(mapping, clauses, qs, p::Union{typeof(⊥), Atom, Literal, Tree{<:Union{typeof(⊥), typeof(∨)}}})
    clause, rs = Literal[], Tree[p]
    while !isempty(rs)
        x = last(_flatten!(clause, rs, pop!(rs)))
        x isa Tree && return mapping, clauses, push!(qs, p)
    end

    _clause = Set{Int}()
    for literal in clause
        _literal = (nodevalue(literal) == 𝒾 ? 1 : -1) * get!(() -> length(mapping) + 1, mapping, child(literal))
        -_literal in _clause ? (return mapping, clauses, push!(qs, p)) : push!(_clause, _literal)
    end

    mapping, push!(clauses, _clause), qs
end
function flatten!(mapping, clauses, qs, p::Tree{typeof(∧)})
    for node in p.nodes
        flatten!(mapping, clauses, qs, node)
    end
    mapping, clauses, qs
end
flatten!(mapping, clauses, qs, p) = mapping, clauses, push!(qs, p)

"""
    flatten(p)
"""
function flatten(p::Tree)
    mapping, clauses, qs = flatten!(Dict{Atom, Int}(), Set{Set{Int}}(), Tree[], p)
    Normal(∧, map(first, sort!(collect(mapping); by = last)), clauses), qs
end
flatten(p) = flatten(Tree(p))

# Instantiation

"""
    @atomize(expression)

Instantiate [`Constant`](@ref)s and [`Variable`](@ref)s inline.

Constants are instantiated with the `\$` interpolation syntax.
Variables are instantiated with previously undefined symbols.

!!! warning
    This macro attempts to ignore symbols that are being assigned a value.
    For example, `@atomize f(; x = p) = x ∧ q` should be equivalent to
    `f(; x = @atomize p)) = x ∧ @atomize q`.
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
    @variables(ps...)

Define [`Variable`](@ref)s and return a vector containing them.

Each symbol `p` is defined as `p = PAndQ.Variable(:p)`.

Examples
```jldoctest
julia> @variables p q
2-element Vector{PAndQ.Variable}:
 p
 q

julia> p
p

julia> q
q
```
"""
macro variables(ps...) esc(quote
    $(map(p -> :($p = $(Variable(p))), ps)...)
    [$(ps...)]
end) end

"""
    constants(xs)

Instantiate each element as a [`Constant`](@ref).

# Examples
```jldoctest
julia> constants(1:2)
2-element Vector{PAndQ.Constant{Int64}}:
 \$(1)
 \$(2)

julia> constants([[1,2], [3,4]])
2-element Vector{PAndQ.Constant{Vector{Int64}}}:
 \$([1, 2])
 \$([3, 4])
```
"""
constants(xs) = map(Constant, xs)

# Utility

"""
    value(p)

If `p` is logically equivalent to a [`Constant`](@ref), return that value wrapped in `Some`.
Otherwise, return nothing.

Values wrapped in `Some` can be unwrapped using the `something` function.

# Examples
```jldoctest
julia> @atomize value(p)

julia> @atomize value(\$1)
Some(1)

julia> @atomize something(value(\$2))
2
```
"""
function value(p)
    _atoms = atoms(p)
    if isempty(_atoms) nothing
    else
        atom = first(_atoms)
        atom isa Constant && atom == p ? Some(atom.value) : nothing
    end
end

"""
    atoms(p)

Return an iterator of each [`Atom`](@ref) contained in `p`.

# Examples
```jldoctest
julia> @atomize collect(atoms(p ∧ q))
2-element Vector{PAndQ.Variable}:
 p
 q
```
"""
atoms(p) = Iterators.filter(leaf -> leaf isa Atom, Leaves(p))

"""
    operators(p)

Return an iterator of each [operator]
(@ref operators_operators) contained in `p`.

# Examples
```jldoctest
julia> @atomize collect(operators(¬p))
1-element Vector{Operator{:not}}:
 ¬

julia> @atomize collect(operators(¬p ∧ q))
3-element Vector{Operator}:
 ∧
 ¬
 𝒾
```
"""
operators(p) = Iterators.filter(node -> !isa(node, Atom), nodevalues(PreOrderDFS(p)))

"""
    install_atomize_mode(;
        start_key = "\\M-a", prompt_text = "atomize> ", prompt_color = :cyan,
    kwargs...)

Install the `atomize` REPL mode, where input implicitly begins with [`@atomize`](@ref).

Keyword arguments are passed to [`ReplMaker.initrepl`](https://github.com/MasonProtter/ReplMaker.jl).
The default start keys are pressing both the \\[Meta\\] (also known as [Alt]) and [a] keys at the same time.
The available `prompt_color`s are in `Base.text_colors`.
"""
function install_atomize_mode(; start_key = "\\M-a", prompt_text = "atomize> ", prompt_color = :cyan, kwargs...)
    initrepl(atomize ∘ Meta.parse;
        prompt_text,
        start_key,
        prompt_color,
        mode_name = :atomize,
        valid_input_checker = complete_julia,
        startup_text = false,
        kwargs...
    )
    @info "The `atomize` REPL mode has been installed: press [$start_key] to enter and [Backspace] to exit"
end

# Normalization

"""
    normalize(::Union{typeof(¬), typeof(∧), typeof(∨)}, p)

Convert the given proposition to negation, conjunction, or disjunction normal form depending
on whether the first argument is [`not`](@ref), [`and`](@ref), or [`or`](@ref), respectively.

Considering the syntax tree of a normalized proposition, each leaf
node is a literal; either an [`Atom`](@ref) or it's negation.
Propositions in negation normal form are expanded such that the
syntax tree branches only contain the operators `and` and `or`.
Conjunction and disjunction normal forms are negated normal forms that have
been flattened by recursively distributing either the `and` or `or` operator over the other.
In other words, a collection of literals is a clause and
a proposition in conjunctive or disjunctive normal form is a conjunction of
disjunctive clauses or a disjunction of conjunctive clauses, respectively.

Conjunctive and disjunctive, but not negation, normal forms are called *canonical*.
Distributing an operator during conversion increases the size of the syntax tree exponentially.
Therefore, it is not possible to compute the canonical form for sufficiently large propositions.
Use the [`tseytin`](@ref) transformation to find a proposition in conjunctive normal form which [`is_equisatisfiable`](@ref).

Operations between canonical propositions return another canonical proposition,
while operations between canonical and non-canonical propositions return a non-canonical proposition.
It is performant to apply the `not` operator to a proposition in canonical normal form
and the `and` or `or` operator to two propositions in conjunction or disjunction normal form, respectively.
It is not performant to convert a proposition between conjunction and disjunction normal form.
Therefore, it is typically more performant to first perform operations on
non-canonical propositions before converting them to a canonical form.

# Examples
```jldoctest
julia> @atomize normalize(∧, ¬(p ∨ q))
(¬p) ∧ (¬q)

julia> @atomize normalize(∨, p ↔ q)
(¬q ∧ ¬p) ∨ (q ∧ p)
```
"""
normalize(::typeof(¬), p::Tree) = something(negated_normal(p))
function normalize(::typeof(∧), p::Tree)
    q, rs = flatten(p)
    q ∧ first(flatten(something(fold(r -> distribute(normalize(¬, r)), (∧) => rs))))
end
normalize(::typeof(∨), p) = ¬normalize(∧, ¬p)
normalize(operator, p) = normalize(operator, Tree(p))

_tseytin(p::Union{Atom, Tree{typeof(𝒾), <:Atom}}) = p
_tseytin(p) = Variable(gensym())

tseytin!(pairs, substitution, ::Union{Atom, Tree{typeof(𝒾), <:Atom}}) = pairs
function tseytin!(pairs, substitution, p)
    nodes = p.nodes
    substitutions = map(_tseytin, nodes)
    push!(pairs, (substitution, nodevalue(p)(map(_tseytin, substitutions)...)))
    for (substitution, node) in zip(substitutions, nodes)
        tseytin!(pairs, substitution, node)
    end
    pairs
end

"""
    tseytin(p)

Apply the [Tseytin transformation](https://en.wikipedia.org/wiki/Tseytin_transformation)
to the given proposition.

The transformed proposition is [`normalize`](@ref)d to conjunction form,
contains introduced [`Variable`](@ref)s,
and [`is_equisatisfiable`](@ref) to `p`.
The [`valuations`](@ref) of the transformed proposition that result
in a true interpretation are a subset of the same for `p`.

# Examples
```jldoctest
julia> is_equisatisfiable(⊤, tseytin(⊤))
true

julia> @atomize is_equisatisfiable(p, tseytin(p))
true

julia> is_equisatisfiable(⊥, tseytin(⊥))
true
```
"""
function tseytin(p::Tree)
    pairs = tseytin!(NTuple{2, Tree}[], Variable(gensym()), Tree(p))
    normalize(∧, isempty(pairs) ? p : first(first(pairs)) ∧ ⋀(map(splat(↔), pairs)))
end
tseytin(p) = tseytin(Tree(p))

"""
    dimacs(io = stdout, p)

# Examples
```jldoctest
julia> @atomize dimacs(p ↔ q)
p cnf 2 2
1 -2 0
-1 2 0

julia> @atomize dimacs(String, p ↔ q)
"p cnf 2 2\\n1 -2 0\\n-1 2 0\\n"
```
"""
dimacs(io, p::Normal{typeof(∧)}) = PicoSAT.dimacs(io, p.clauses)
dimacs(io, p) = dimacs(io, normalize(∧, p))
dimacs(p) = dimacs(stdout, p)
