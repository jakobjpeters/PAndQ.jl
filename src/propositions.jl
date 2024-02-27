
import AbstractTrees: HasNodeType, NodeType, children, nodetype, nodevalue, printnode
import Base: map
using AbstractTrees: Leaves, PreOrderDFS, childtype, nodevalues
using Base.Iterators: Stateful
using Base: isexpr, parse
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
    Tree{N, AT <: Union{Atom, Tree}} <: Compound
    Tree(::UnaryOperator, ::Atom)
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
struct Tree{N, P <: Proposition} <: Compound
    operator::Operator
    propositions::NTuple{N, P}

    Tree(o::UnaryOperator, p::A) where A <: Atom = new{1, A}(o, (p,))
    Tree(o::O, ps::Tree...) where O <: Operator = new{arity(o), Tree}(o, ps)
end

"""
    Clause{AO <: AndOr} <: Compound
    Clause(::AO, ::Vector{Atom}, ::Set{Int})

A proposition represented as either a [conjunction or disjunction of literals]
(https://en.wikipedia.org/wiki/Clause_(logic)).

!!! info
    An empty `Clause` is [logically equivalent](@ref ==) to the
    neutral element of it's binary operator.

Subtype of [`Compound`](@ref).
See also [`Atom`](@ref), [`AndOr`](@ref), and [`NullaryOperator`](@ref).

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
struct Normal{AO <: AndOr} <: Compound
    atoms::Vector{Atom}
    clauses::Set{Set{Int}}

    Normal(::AO, atoms::Vector{<:Atom}, clauses) where AO = new{AO}(atoms, clauses)
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
(identical(PAndQ.Variable(:p)), identical(PAndQ.Variable(:q)))
```
"""
children(p::Tree) = p.propositions
children(p::Clause) = Iterators.map(i -> Tree(signbit(i) ? (¬) : 𝒾, p.atoms[abs(i)]), p.literals)
children(p::Normal) = Iterators.map(p.clauses) do clause
    atoms = p.atoms[collect(Iterators.map(abs, clause))]
    Clause(dual(nodevalue(p)), atoms, Set(Iterators.map(literal -> sign(literal) * findfirst(==(p.atoms[abs(literal)]), atoms), clause)))
end

"""
    nodevalue(::Union{Tree, Clause, Normal})

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

_printnode(p::Union{Operator, Atom}) = p
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
printnode(io::IO, p::Union{Operator, Proposition}; kwargs...) =
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
atomize(x) =
    if x isa Symbol; :((@isdefined $x) ? $x : $(Variable(x)))
    elseif x isa Expr
        if length(x.args) == 0 || (isexpr(x, :macrocall) && first(x.args) == Symbol("@atomize")) x
        elseif isexpr(x, :$); :((@isdefined PAndQ) ? PAndQ.Constant($(only(x.args))) : error("`PAndQ` must be loaded"))
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
        o, rs = nodevalue(q), children(q)

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

Given a proposition in negated normal form, return that proposition to conjunction normal form.
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
            o, ss = nodevalue(r), children(r)

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
macro atomize(x)
    esc(:($(atomize(x))))
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
    constants(f = 𝒾, xs)

Apply `f` and then instantiate each element as a [`Constant`](@ref).

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
    value(p, T = Any)

If `p` is logically equivalent to a [`Constant`](@ref), return that value wrapped in `Some`.
Otherwise, return nothing.

Values wrapped in `Some` can be unwrapped using the `something` function.

!!! tip
    To reduce compilation latency, constants do not store the type of the wrapped value.
    Therefore, the type of this value cannot be inferred and can result in run-time dispatch.
    If this type is known at compile-time, pass it as the second parameter.
    See also [Annotate values taken from untyped locations]
    (https://docs.julialang.org/en/v1/manual/performance-tips/#Annotate-values-taken-from-untyped-locations).

# Examples
```jldoctest
julia> @atomize value(p)

julia> @atomize value(\$1)
Some(1)

julia> @atomize something(value(\$2, Int))
2
```
"""
function value(p, T = Any)
    _atoms = atoms(p)
    if isempty(_atoms) nothing
    else
        atom = first(_atoms)
        atom isa Constant && atom == p ? Some(atom.value::T) : nothing
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
function normalize(::typeof(¬), p::Tree)
    operator_stack, input_stack, output_stack = Pair{Int, Operator}[], Tree[p], Tree[]

    while !isempty(input_stack)
        q = pop!(input_stack)
        o, rs = nodevalue(q), children(q)

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
        o, qs = nodevalue(p), children(p)
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
    pairs = tseytin!(NTuple{2, Tree}[], Variable(gensym()), p)
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
