
import AbstractTrees: children, nodevalue, printnode
import Base: ==, hash, map
using AbstractTrees: Leaves, PreOrderDFS, childtype, nodevalues
using Base.Iterators: Stateful
using Base: isexpr
using ReplMaker: complete_julia, initrepl

"""
    Kind
"""
@enum Kind constant operator variable

## Types

"""
    AbstractSyntaxTree

A [proposition](https://en.wikipedia.org/wiki/Proposition)
represented by an [abstract syntax tree]
(https://en.wikipedia.org/wiki/Abstract_syntax_tree).

See also [`Operator`](@ref).

# Examples
```jldoctest
julia> PAndQ.AbstractSyntaxTree(⊤)
⊤

julia> @atomize PAndQ.AbstractSyntaxTree(¬, [p])
¬p

julia> @atomize PAndQ.AbstractSyntaxTree(and, [PAndQ.AbstractSyntaxTree(p), PAndQ.AbstractSyntaxTree(q)])
p ∧ q
```
"""
struct AbstractSyntaxTree
    kind::Kind
    value
    branches::Vector{AbstractSyntaxTree}
end

AbstractSyntaxTree(k::Kind, v) = AbstractSyntaxTree(k, v, AbstractSyntaxTree[])
AbstractSyntaxTree(o::Operator, ps) = AbstractSyntaxTree(operator, o, ps)

## AbstractTrees.jl

"""
    children(::AbstractSyntaxTree)

Return an iterator over the child nodes of the given proposition.

See also [`AbstractSyntaxTree`](@ref).

# Examples
```jldoctest
julia> @atomize PAndQ.children(¬p)
1-element Vector{PAndQ.AbstractSyntaxTree}:
 p

julia> @atomize PAndQ.children(p ∧ q)
2-element Vector{PAndQ.AbstractSyntaxTree}:
 p
 q
```
"""
children(p::AbstractSyntaxTree) = p.branches

"""
    nodevalue(::AbstractSyntaxTree)

Return the [`Operator`](@ref Interface.Operator) of the proposition's root node.

See also [`AbstractSyntaxTree`](@ref).

# Examples
```jldoctest
julia> @atomize PAndQ.nodevalue(¬p)
¬

julia> @atomize PAndQ.nodevalue(p ∧ q)
∧
```
"""
nodevalue(p::AbstractSyntaxTree) = p.kind == operator ? p.value : p

"""
    printnode(::IO, ::Union{Operator, AbstractSyntaxTree}; kwargs...)

Print the representation of the proposition's root node.

See also [`Operator`](@ref Interface.Operator) and [`AbstractSyntaxTree`](@ref).

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
printnode(io::IO, p::AbstractSyntaxTree) = p.kind == operator ?
    show(io, MIME"text/plain"(), nodevalue(p)) :
    show(io, MIME"text/plain"(), p)

## Utilities

"""
    ::AbstractSyntaxTree == ::AbstractSyntaxTree
"""
function ==(p::AbstractSyntaxTree, q::AbstractSyntaxTree)
    p_kind, q_kind = p.kind, q.kind
    p_kind == q_kind && p.branches == q.branches &&
        if p_kind == variable p.value::Symbol == q.value::Symbol
        elseif p_kind == operator p.value::Operator == q.value::Operator
        else p.value == q.value
        end
end

"""
    hash(::AbstractSyntaxTree, ::UInt)
"""
hash(p::AbstractSyntaxTree, h::UInt) =
    hash(AbstractSyntaxTree, hash(p.kind, hash(p.value, hash(p.branches, h))))

"""
    deconstruct(p)

Return `(nodevalue(p), children(p))`.

See also [`nodevalue`](@ref) and [`children`](@ref).

# Examples
```jldoctest
julia> @atomize PAndQ.deconstruct(p)
(PAndQ.AbstractSyntaxTree(:p), PAndQ.AbstractSyntaxTree[])

julia> @atomize PAndQ.deconstruct(¬p)
(not, PAndQ.AbstractSyntaxTree[PAndQ.AbstractSyntaxTree(:p)])

julia> @atomize PAndQ.deconstruct(p ∧ q)
(and, PAndQ.AbstractSyntaxTree[PAndQ.AbstractSyntaxTree(:p), PAndQ.AbstractSyntaxTree(:q)])
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
    atomize(x)

If `x` is a symbol, return an expression that instantiates it as a
variable if it is undefined in the caller's scope.
If `isexpr(x, :\$)`, return an expression that instantiates it as a constant.
If `x` is a different expression, traverse it with recursive calls to `atomize`.
Otherise, return x.
"""
atomize(x) =
    if x isa Symbol; :((@isdefined $x) ? $x : $(AbstractSyntaxTree(variable, x)))
    elseif x isa Expr
        if length(x.args) == 0 || (isexpr(x, :macrocall) && first(x.args) == Symbol("@atomize")) x
        elseif isexpr(x, :$); :($AbstractSyntaxTree($constant, Some($(only(x.args)))))
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
    p = AbstractSyntaxTree(initial_value(ao))

    while !isempty(stack)
        q = pop!(stack)
        o, rs = deconstruct(q)

        if o in [⊤, ⊥] return AbstractSyntaxTree(o)
        elseif o isa AbstractSyntaxTree || o == (¬) p = evaluate(ao, [p, q])
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
distribute(p) = _distribute((q, rs, conjuncts) -> evaluate(∧, [q, _distribute(∨, map(AbstractSyntaxTree, rs)) do s, ts, disjuncts
    u = evaluate(∨, [s, fold(identity, (∨) => disjuncts)])
    empty!(disjuncts)
    append!(conjuncts, map(t -> t ∨ u, ts))
    AbstractSyntaxTree(⊤)
end]), ∧, AbstractSyntaxTree[normalize(¬, p)])

"""
    prune(p, atoms = AbstractSyntaxTree[], mapping = Dict{AbstractSyntaxTree, Int}())
"""
function prune(p, atoms = AbstractSyntaxTree[], mapping = Dict{Union{Some, Symbol}, Int}())
    clauses, qs, stack = Set{Set{Int}}(), AbstractSyntaxTree[], AbstractSyntaxTree[p]

    while !isempty(stack)
        r = pop!(stack)
        o = nodevalue(r)

        if o == ⊤
        elseif o == ⊥
            push!(empty!(clauses), Set{Int}())
            empty!(qs)
            break
        elseif o == (∧) append!(stack, children(r))
        elseif o isa AbstractSyntaxTree || o in [¬, ∨]
            clause, _stack = Set{Int}(), AbstractSyntaxTree[r]

            while !isempty(_stack)
                s = pop!(_stack)
                _o, ts = deconstruct(s)

                if _o == ⊥
                elseif s.kind != operator || ((_o == ¬) && only(ts).kind != operator)
                    atom = s.kind == operator ? only(ts) : s
                    literal = (_o isa AbstractSyntaxTree || _o == 𝒾 ? 1 : -1) * get!(mapping, atom.value) do
                        push!(atoms, atom)
                        length(mapping) + 1
                    end

                    if -literal in clause
                        empty!(clause)
                        break
                    else push!(clause, literal)
                    end
                elseif _o == (∨) append!(_stack, ts)
                else
                    push!(qs, r)
                    empty!(clause)
                    break
                end
            end

            isempty(clause) || push!(clauses, clause)
        else push!(qs, r)
        end
    end

    clauses, atoms, mapping, qs
end

"""
    reconstruct(clauses, atoms)
"""
reconstruct(clauses, atoms) = fold(clause -> fold(
    literal -> (signbit(literal) ? (¬) : 𝒾)(AbstractSyntaxTree(atoms[abs(literal)])), (∨) => clause), (∧) => clauses)

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
2-element Vector{PAndQ.AbstractSyntaxTree}:
 p
 q

julia> p
p

julia> q
q
```
"""
macro variables(ps...) esc(quote
    $(map(p -> :($p = $(AbstractSyntaxTree(variable, p))), ps)...)
    $(:($AbstractSyntaxTree[$(ps...)]))
end) end

"""
    constants(f = identity, xs)

Equivalent to `map(x -> @atomize \$(f(x)), xs)`.

See also [`identical`](@ref).

# Examples
```jldoctest
julia> constants(1:2)
2-element Vector{PAndQ.AbstractSyntaxTree}:
 \$(1)
 \$(2)

julia> constants(string, 1:2)
2-element Vector{PAndQ.AbstractSyntaxTree}:
 \$("1")
 \$("2")
```
"""
constants(f, xs) = map(AbstractSyntaxTree ∘ Some ∘ f, xs)
constants(xs) = constants(identity, xs)

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
        _atom = atom.value
        _atom isa Some && atom == p ? _atom : nothing
    end
end
value(p) = value(Any, p)

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
map(f, p::Operator) = p
map(f, p::AbstractSyntaxTree) = p.kind == operator ?
    _evaluation(nodevalue(p), map(q -> q.kind != operator ? f(q) : map(f, q), children(p))) :
    f(p)

"""
    atoms(p)

Return an iterator of each atomic proposition contained in `p`.

# Examples
```jldoctest
julia> @atomize collect(atoms(p ∧ q))
2-element Vector{PAndQ.AbstractSyntaxTree}:
 p
 q
```
"""
atoms(p) = Iterators.filter(leaf -> leaf isa AbstractSyntaxTree && leaf.kind != operator, Leaves(p))

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
        mode_name = :atomize,
        valid_input_checker = complete_julia,
        startup_text = false,
        prompt_text,
        start_key,
        prompt_color,
        kwargs...
    )
    @info "The `atomize` REPL mode has been installed: press [$start_key] to enter and [Backspace] to exit"
end

# Normalization

"""
    normalize(::typeof(¬), p)
    normalize(::Union{typeof(∧), typeof(∨)}, p; canonical = false)

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
¬q ∧ ¬p

julia> @atomize normalize(∨, p ↔ q)
(¬q ∧ ¬p) ∨ (p ∧ q)
```
"""
normalize(o, p) =
    if o == (¬)
        operator_stack, input_stack, output_stack = Pair{Int, Operator}[], AbstractSyntaxTree[p], AbstractSyntaxTree[]

        while !isempty(input_stack)
            q = pop!(input_stack)
            o, rs = deconstruct(q)

            if o in [⊤, ⊥] || q.kind != operator || ((o == ¬) && only(rs).kind != operator) push!(output_stack, q)
            elseif o in [∧, ∨]
                push!(operator_stack, length(input_stack) => o)
                append!(input_stack, rs)
            else push!(input_stack, evaluate(o, rs))
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
                else evaluate(o, [pop!(input_stack), pop!(input_stack)])
                end
            )
        end

        only(input_stack)
    elseif o == (∧)
        clauses, atoms, mapping, qs = prune(p)

        for q in qs
            for clause in first(prune(distribute(q), atoms, mapping))
                push!(clauses, clause)
            end
        end

        reconstruct(clauses, atoms)
    elseif o == (∨) normalize(¬, ¬normalize(∧, ¬p))
    else error()
    end

tseytin!(clauses, atoms, mapping, p) =
    union!(clauses, first(prune(normalize(∧, p), atoms, mapping)))

__tseytin(p) = (p.kind != operator || (nodevalue(p) == 𝒾 && child(p).kind != operator)) ? p : AbstractSyntaxTree(gensym())

function _tseytin(p)
    clauses, atoms, mapping, qs = prune(p)
    stack = NTuple{2, AbstractSyntaxTree}[]

    for q in qs
        substitution = __tseytin(q)
        push!(stack, (q, substitution))
        tseytin!(clauses, atoms, mapping, substitution)

        while !isempty(stack)
            r, substitution = pop!(stack)
            o, ss = deconstruct(r)
            substitutions = map(__tseytin, ss)
            append!(stack, Iterators.filter(((s, substitution),) -> !(s.kind != operator), zip(ss, substitutions)))
            (o == 𝒾 && only(ss).kind != operator) || tseytin!(clauses, atoms, mapping, substitution ↔ AbstractSyntaxTree(o, substitutions))
        end
    end

    clauses, atoms
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
tseytin(p) = reconstruct(_tseytin(p)...)
