
import AbstractTrees: HasNodeType, NodeType, children, nodetype, nodevalue, printnode
import Base: map
using AbstractTrees: Leaves, PreOrderDFS, childtype, nodevalues
using Base.Iterators: Stateful
using Base: isexpr
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
    Constant <: Atom
    Constant(value)

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
struct Constant <: Atom
    value
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
end

"""
    Tree{N} <: Compound
    Tree(::Operator, ::Union{Atom, Tree}...)

A [`Proposition`](@ref) represented by an [abstract syntax tree]
(https://en.wikipedia.org/wiki/Abstract_syntax_tree).

Subtype of [`Compound`](@ref).
See also [`Operator`](@ref) and [`Atom`](@ref).

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
struct Tree{N} <: Compound
    operator::Operator
    propositions::NTuple{N, Union{Atom, Tree}}

    Tree(o::O, ps...) where O <: Operator = new{arity(o)}(o, ps)
end

"""
    Clause{AO <: AndOr} <: Compound
    Clause(::AO, ::Vector{<:Atom}, ::Set{Int})

A proposition represented as either a [conjunction or disjunction of literals]
(https://en.wikipedia.org/wiki/Clause_(logic)).

!!! info
    An empty `Clause` is [logically equivalent](@ref ==) to the
    [`initial_value`](@ref Interface.initial_value) of it's binary operator.

Subtype of [`Compound`](@ref).
See also [`AndOr`](@ref) and [`Atom`](@ref).

# Examples
```jldoctest
julia> PAndQ.Clause(∧, PAndQ.Atom[], Set{Int}())
⊤

julia> @atomize PAndQ.Clause(∧, [p], Set(1))
p

julia> @atomize PAndQ.Clause(∨, [p, q], Set([1, -2]))
¬q ∨ p
```
"""
struct Clause{AO <: AndOr} <: Compound
    atoms::Vector{Atom}
    literals::Set{Int}

    Clause(::AO, atoms::Vector{<:Atom}, literals) where AO <: AndOr = new{AO}(atoms, literals)
end

"""
    Normal{AO <: AndOr} <: Compound
    Normal(::AO, ::Vector{Atom}, ::Set{Set{Int}})

A [`Proposition`](@ref) represented in [conjunctive]
(https://en.wikipedia.org/wiki/Conjunctive_normal_form) or [disjunctive]
(https://en.wikipedia.org/wiki/Disjunctive_normal_form) normal form.

!!! info
    An empty `Normal` is [logically equivalent](@ref ==) to the
    [`initial_value`](@ref Interface.initial_value) of it's binary operator.

Subtype of [`Compound`](@ref).
See also [`AndOr`](@ref) and [`Clause`](@ref).

# Examples
```jldoctest
julia> PAndQ.Normal(∧, PAndQ.Atom[], Set{Set{Int}}())
⊤

julia> @atomize PAndQ.Normal(∧, [p, q], Set(map(Set, ((1, 2), (-1, -2)))))
(¬p ∨ ¬q) ∧ (p ∨ q)
```
"""
struct Normal{AO <: AndOr} <: Compound
    atoms::Vector{Atom}
    clauses::Set{Set{Int}}

    Normal(::AO, atoms::Vector{<:Atom}, clauses) where AO = new{AO}(atoms, clauses)
end

## AbstractTrees.jl

"""
    children(::Union{Tree, Clause, Normal})

Return an iterator over the child nodes of the given proposition.

See also [`Tree`](@ref), [`Clause`](@ref), and [`Normal`](@ref).

# Examples
```jldoctest
julia> @atomize PAndQ.children(p)
()

julia> @atomize PAndQ.children(¬p)
(PAndQ.Variable(:p),)

julia> @atomize PAndQ.children(p ∧ q)
(identical(PAndQ.Variable(:p)), identical(PAndQ.Variable(:q)))
```
"""
children(p::Tree) = p.propositions
function children(p::Clause)
    atoms = p.atoms
    Iterators.map(literal -> (signbit(literal) ? (¬) : Tree)(atoms[abs(literal)]), p.literals)
end
children(p::Normal) = Iterators.map(p.clauses) do clause
    atoms = p.atoms[collect(Iterators.map(abs, clause))]
    Clause(dual(nodevalue(p)), atoms, Set(Iterators.map(literal -> sign(literal) * findfirst(==(p.atoms[abs(literal)]), atoms), clause)))
end

"""
    nodevalue(::Union{Tree, Clause, Normal})

Return the [`Operator`](@ref Interface.Operator) of the proposition's root node.

See also [`Tree`](@ref), [`Clause`](@ref), and [`Normal`](@ref).

# Examples
```jldoctest
julia> @atomize PAndQ.nodevalue(¬p)
¬

julia> @atomize PAndQ.nodevalue(p ∧ q)
∧
```
"""
nodevalue(p::Tree) = p.operator
nodevalue(::Union{Clause{O}, Normal{O}}) where O = O.instance

_printnode(p::Union{Operator, Atom, Tree}) = nodevalue(p)
_printnode(p::Union{Clause, Normal}) =
    (isempty(children(p)) ? something ∘ initial_value : identity)(nodevalue(p))

"""
    printnode(::IO, ::Union{Operator, Proposition}; kwargs...)

Print the representation of the proposition's root node.

See also [`Operator`](@ref Interface.Operator) and [`Proposition`](@ref).

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
printnode(io::IO, p::Union{Operator, Proposition}) = show(io, MIME"text/plain"(), _printnode(p))

"""
    NodeType(::Type{<:Atom})

See also [`Atom`](@ref).

# Examples
```jldoctest
julia> PAndQ.NodeType(PAndQ.Atom)
AbstractTrees.HasNodeType()
```
"""
NodeType(::Type{<:Atom}) = HasNodeType()

"""
    nodetype(::Type{<:Atom})

See also [`Atom`](@ref).

```jldoctest
julia> PAndQ.nodetype(PAndQ.Atom)
PAndQ.Atom
```
"""
nodetype(::Type{A}) where A <: Atom = A

## Utilities

"""
    deconstruct(p)

Return `(nodevalue(p), children(p))`.

See also [`nodevalue`](@ref) and [`children`](@ref).

# Examples
```jldoctest
julia> @atomize PAndQ.deconstruct(p)
(PAndQ.Variable(:p), ())

julia> @atomize PAndQ.deconstruct(¬p)
(PAndQ.Interface.Operator{:not}(), (PAndQ.Variable(:p),))

julia> @atomize PAndQ.deconstruct(p ∧ q)
(PAndQ.Interface.Operator{:and}(), (identical(PAndQ.Variable(:p)), identical(PAndQ.Variable(:q))))
```
"""
deconstruct(p) = nodevalue(p), children(p)

"""
    child(x)

Equivalent to `only ∘ children`.

See also [`children`](@ref).
"""
child(x) = only(children(x))

"""
    load_or_error(x)

Return an expression that when `eval`uated returns `x` if `PAndQ` is defined and throws an `Exception` otherwise.
"""
load_or_error(x) = :((@isdefined PAndQ) ? $x : error("`PAndQ` must be loaded"))

"""
    atomize(x)

If `x` is a symbol, return an expression that instantiates it as a
[`Variable`](@ref) if it is undefined in the caller's scope.
If `isexpr(x, :\$)`, return an expression that instantiates it as a [`Constant`](@ref).
If `x` is a different expression, traverse it with recursive calls to `atomize`.
Otherise, return x.
"""
atomize(x) =
    if x isa Symbol; :((@isdefined $x) ? $x : $(Variable(x)))
    elseif x isa Expr
        if length(x.args) == 0 || (isexpr(x, :macrocall) && first(x.args) == Symbol("@atomize")) x
        elseif isexpr(x, :$); load_or_error(:(PAndQ.Constant($(only(x.args)))))
        elseif isexpr(x, :kw) Expr(x.head, x.args[1], atomize(x.args[2]))
        elseif isexpr(x, (:struct, :where)) x # TODO
        else # TODO
            y = Expr(x.head)
            b = isexpr(x, (:<:, :(=), :->, :function))
            b && push!(y.args, x.args[1])
            for arg in x.args[1 + b:end]
                push!(y.args, atomize(arg))
            end
            y
        end
    else x
    end

function _distribute(f, ao, stack)
    _initial_value = something(initial_value(ao))
    p, not_initial_value = Tree(_initial_value), dual(_initial_value)

    while !isempty(stack)
        q = pop!(stack)
        o, rs = deconstruct(q)

        if o == not_initial_value return Tree(not_initial_value)
        elseif o isa UnaryOperator p = evaluate(ao, p, q)
        elseif o == ao append!(stack, rs)
        else p = f(p, rs, stack)
        end
    end

    p
end

"""
    distribute(p)

Given a proposition in negation normal form, return that proposition in conjunction normal form.
"""
distribute(p) = _distribute((q, rs, conjuncts) -> evaluate(∧, q, _distribute(∨, Tree[rs...]) do s, ts, disjuncts
    u = evaluate(∨, s, fold(𝒾, (∨) => disjuncts))
    empty!(disjuncts)
    append!(conjuncts, map(t -> t ∨ u, ts))
    Tree(⊤)
end), ∧, Tree[p])

function flatten!(mapping, clauses, qs, p)
    x = nodevalue(p)

    if x isa typeof(⊤)
    elseif x isa Union{typeof(⊥), UnaryOperator, typeof(∨), Atom}
        clause, rs = Tree[], Tree[p]
        while !isempty(rs)
            r = pop!(rs)
            o, ss = deconstruct(r)

            if o isa typeof(⊥)
            elseif o isa UnaryOperator && only(ss) isa Atom push!(clause, r)
            elseif o isa typeof(∨) append!(rs, ss)
            else return mapping, clauses, push!(qs, p)
            end
        end

        _clause = Set{Int}()
        for literal in clause
            _literal = (nodevalue(literal) == 𝒾 ? 1 : -1) * get!(() -> length(mapping) + 1, mapping, child(literal))
            -_literal in _clause ? (return mapping, clauses, push!(qs, p)) : push!(_clause, _literal)
        end

        push!(clauses, _clause)
    elseif x isa typeof(∧)
        for r in children(p)
            flatten!(mapping, clauses, qs, r)
        end
    else push!(qs, p)
    end

    mapping, clauses, qs
end

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
    @atomize(x)

Instantiate constants and variables inline.

Constants are instantiated with the `\$` interpolation syntax.
Variables are instantiated with previously undefined symbols.

!!! warning
    This macro attempts to ignore symbols that are being assigned a value.
    For example, `@atomize f(; x = p) = x ∧ q` should be equivalent to
    `f(; x = @atomize p) = x ∧ @atomize q`.
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
macro atomize(x)
    esc(:($(atomize(x))))
end

"""
    @variables(ps...)

Define variables and return a `Vector` containing them.

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
    $(load_or_error(:(PAndQ.Variable[$(ps...)])))
end) end

"""
    constants(f = 𝒾, xs)

Equivalent to `map(x -> @atomize \$(f(x)), xs)`.

See also [`identical`](@ref).

# Examples
```jldoctest
julia> constants(1:2)
2-element Vector{PAndQ.Constant}:
 \$(1)
 \$(2)

julia> constants(string, 1:2)
2-element Vector{PAndQ.Constant}:
 \$("1")
 \$("2")
```
"""
constants(f, xs) = map(Constant ∘ f, xs)
constants(xs) = constants(𝒾, xs)

# Utility

"""
    value(T = Any, p)

If `p` is logically equivalent to a constant, return that constant's value wrapped in `Some`.
Otherwise, return `nothing`.

Values wrapped in `Some` can be unwrapped using the `something` function.

!!! tip
    To reduce compilation latency, constants do not store the type of the wrapped value.
    Therefore, the type of this value cannot be inferred and can result in run-time dispatch.
    If this type is known at compile-time, pass it as the first parameter.
    See also the performance tip to [Annotate values taken from untyped locations]
    (https://docs.julialang.org/en/v1/manual/performance-tips/#Annotate-values-taken-from-untyped-locations).

# Examples
```jldoctest
julia> @atomize value(p)

julia> @atomize value(\$1)
Some(1)

julia> @atomize something(value(Int, \$2))
2
```
"""
function value(T, p)
    _atoms = atoms(p)
    if isempty(_atoms) nothing
    else
        atom = first(_atoms)
        atom isa Constant && atom == p ? Some(atom.value::T) : nothing
    end
end
value(p) = value(Any, p)

_map(f, p) = map(child -> map(f, child), children(p))

"""
    map(f, p)

Apply the function `f` to each atom in `p`.

# Examples
```jldoctest
julia> @atomize map(¬, p ∧ q)
¬p ∧ ¬q

julia> @atomize map(atom -> \$(something(value(atom)) + 1), \$1 ∧ \$2)
\$(2) ∧ \$(3)
```
"""
map(f, p::Atom) = f(p)
map(f, p::Union{NullaryOperator, Tree}) = nodevalue(p)(_map(f, p)...)
map(f, p::Union{Clause, Normal}) = fold(𝒾, nodevalue(p) => _map(f, p))

"""
    atoms(p)

Return an iterator of each atomic proposition contained in `p`.

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
1-element Vector{PAndQ.Interface.Operator{:not}}:
 ¬

julia> @atomize collect(operators(¬p ∧ q))
3-element Vector{PAndQ.Interface.Operator}:
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
node is a literal; either an atom or it's negation.
Propositions in negation normal form are expanded such that the
syntax tree branches only contain the operators `and` and `or`.
Conjunction and disjunction normal forms are negated normal forms that have
been flattened by recursively distributing either the `and` or `or` operator over the other.
In other words, a collection of literals is a clause and
a proposition in conjunctive or disjunctive normal form is a conjunction of
disjunctive clauses or a disjunction of conjunctive clauses, respectively.

Conjunction and disjunction, but not negation, normal forms are called *canonical*.
Distributing an operator during conversion may increase the size of the syntax tree exponentially.
Therefore, it may be intractable to compute a canonical form for sufficiently large propositions. Instead,
use the [`tseytin`](@ref) transformation to find a proposition in conjunctive normal form which
[`is_equisatisfiable`](@ref) to the given proposition.

!!! tip
    Converting a proposition between conjunction and disjunction normal form is not performant
    due to the exponential increase in the size of the proposition.
    It is most performant to apply all operations to propositions and normalizing the resulting proposition once.

# Examples
```jldoctest
julia> @atomize normalize(∧, ¬(p ∨ q))
(¬p) ∧ (¬q)

julia> @atomize normalize(∨, p ↔ q)
(¬q ∧ ¬p) ∨ (q ∧ p)
```
"""
function normalize(::typeof(¬), p::Tree)
    operator_stack, input_stack, output_stack = Pair{Int, Operator}[], Tree[p], Tree[]

    while !isempty(input_stack)
        q = pop!(input_stack)
        o, rs = deconstruct(q)

        if o isa NullaryOperator || (o isa UnaryOperator && only(rs) isa Atom) push!(output_stack, q)
        elseif o isa AndOr
            push!(operator_stack, length(input_stack) => o)
            append!(input_stack, rs)
        else push!(input_stack, evaluate(o, rs...))
        end
    end

    isempty(operator_stack) && append!(input_stack, output_stack)

    while !isempty(operator_stack)
        i, o = pop!(operator_stack)
        _arity = arity(o)

        while i + _arity > length(input_stack)
            push!(input_stack, pop!(output_stack))
        end

        push!(input_stack,
            if _arity == 0 o()
            elseif _arity == 1 o(pop!(input_stack))
            else evaluate(o, pop!(input_stack), pop!(input_stack))
            end
        )
    end

    only(input_stack)
end
function normalize(::typeof(∧), p::Tree)
    q, rs = flatten(p)
    q ∧ first(flatten(something(fold(r -> distribute(normalize(¬, r)), (∧) => rs))))
end
normalize(::typeof(∨), p) = ¬normalize(∧, ¬p)
normalize(::AO, p::Normal{AO}) where AO <: AndOr = p
normalize(o, p) = normalize(o, Tree(p))

__tseytin(::typeof(𝒾), p::Atom) = Tree(𝒾, p)
__tseytin(o, ps...) = Variable(gensym())

_tseytin(p::Atom) = p
_tseytin(p) = __tseytin(nodevalue(p), children(p)...)

function tseytin!(pairs, substitution, p)
    if !isa(p, Atom)
        o, qs = deconstruct(p)
        if !(o isa typeof(𝒾) && only(qs) isa Atom)
            substitutions = map(_tseytin, qs)
            push!(pairs, (substitution, o(substitutions...)))

            for (substitution, q) in zip(substitutions, qs)
                tseytin!(pairs, substitution, q)
            end
        end
    end

    pairs
end

"""
    tseytin(p)

Apply the [Tseytin transformation](https://en.wikipedia.org/wiki/Tseytin_transformation)
to the given proposition.

Using the [`normalize`](@ref) function to convert a proposition to conjunction normal form
may result in an exponentially larger proposition, which can be intractable for sufficiently large propositions.
The Tseytin transformation results in a linearly larger proposition that is in conjunction normal form
and [`is_equisatisfiable`](@ref) to the original.
However, it contains introduced variables and yields a subset of the [`solutions`](@ref) to the original proposition.

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
    pairs = tseytin!(NTuple{2, Tree}[], Variable(gensym()), p)
    normalize(∧, isempty(pairs) ? p : first(first(pairs)) ∧ ⋀(map(splat(↔), pairs)))
end
tseytin(p) = tseytin(Tree(p))
