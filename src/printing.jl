
import Base: Stateful, show
using AbstractTrees: AbstractTrees
using Base.Docs: HTML
using Base.Iterators: flatmap
using PrettyTables: pretty_table

"""
    TruthTable(ps)

Construct a [truth table](https://en.wikipedia.org/wiki/Truth_table)
for the given propositions.

The header is a sequence of propositions.
The body is a matrix where the rows are [`interpretations`](@ref) of each proposition in the header.

Propositions logically equivalent to a [truth value](@ref nullary_operators) will be grouped on the left,
followed by those equivalent to an atomic proposition, and then by all other propositions.
Logically equivalent propositions will be grouped together.
Propositions that have the same text representation will only be shown once.

# Examples
```jldoctest
julia> TruthTable([⊤])
┌───┐
│ ⊤ │
├───┤
│ ⊤ │
└───┘

julia> @atomize TruthTable([¬p])
┌───┬────┐
│ p │ ¬p │
├───┼────┤
│ ⊤ │ ⊥  │
│ ⊥ │ ⊤  │
└───┴────┘

julia> @atomize TruthTable([p ∧ ¬p, p → q, ¬p ∨ q])
┌────────┬───┬───┬───────────────┐
│ p ∧ ¬p │ p │ q │ p → q, ¬p ∨ q │
├────────┼───┼───┼───────────────┤
│ ⊥      │ ⊤ │ ⊤ │ ⊤             │
│ ⊥      │ ⊥ │ ⊤ │ ⊤             │
├────────┼───┼───┼───────────────┤
│ ⊥      │ ⊤ │ ⊥ │ ⊥             │
│ ⊥      │ ⊥ │ ⊥ │ ⊤             │
└────────┴───┴───┴───────────────┘
```
"""
struct TruthTable
    header::Vector{String}
    body::Matrix{Bool}

    function TruthTable(@nospecialize ps)
        ps = collect(ps)
        __atoms = unique(p -> p.value, flatmap(atoms, ps))
        ps = append!(ps, __atoms)
        _valuations = vec(collect(valuations(__atoms)))
        _interpretations = Iterators.map(p -> vec(map(
            valuation -> Bool(interpret(valuation, normalize(¬, p))),
        _valuations)), ps)

        truths_interpretations, atoms_interpretations, compounds_interpretations =
            Vector{Bool}[], Vector{Bool}[], Vector{Bool}[]

        grouped_truths = Dict(map(truth -> repeat([truth], length(_valuations)) => AbstractSyntaxTree[], (true, false)))
        grouped_atoms = Dict(map(
            p -> vec(map(Bool, interpretations(_valuations, p))) => AbstractSyntaxTree[],
            __atoms
        ))
        grouped_compounds = Dict{Vector{Bool}, Vector{AbstractSyntaxTree}}()

        for (p, interpretation) in zip(ps, _interpretations)
            _union! = (key, group) -> begin
                union!(key, [interpretation])
                push!(get!(group, interpretation, AbstractSyntaxTree[]), p)
            end

            if interpretation in keys(grouped_truths) _union!(truths_interpretations, grouped_truths)
            elseif interpretation in keys(grouped_atoms) _union!(atoms_interpretations, grouped_atoms)
            else _union!(compounds_interpretations, grouped_compounds)
            end
        end

        header = String[]
        body = Vector{Bool}[]
        for (_interpretations, group) in (
            truths_interpretations => grouped_truths,
            atoms_interpretations => grouped_atoms,
            compounds_interpretations => grouped_compounds
        )
            for interpretation in _interpretations
                xs = get(group, interpretation, AbstractSyntaxTree[])
                push!(header, join(unique!(map(x -> repr("text/plain", x), xs)), ", "))
                push!(body, interpretation)
            end
        end

        new(header, reduce(hcat, body))
    end
end

for (T, f) in (
    NullaryOperator => v -> v ? "⊤" : "⊥",
    String => v -> nameof(v ? "tautology" : "contradiction"),
    Char => v -> v == ⊤ ? "T" : "F",
    Bool => string,
    Int => string ∘ Int
)
    @eval formatter(::Type{$T}) = (v, _, _) -> $f(v)
end

"""
    formatter(T)

Use as the `formatters` keyword parameter in [`print_table`](@ref).

| `T`               | `formatter(T)(true, _, _)` | `formatter(T)(false, _, _)` |
| :---------------- | :------------------------- | :-------------------------- |
| `NullaryOperator` | `"⊤"`                      | `"⊥"`                       |
| `String`          | `"tautology"`              | `"contradiction"`           |
| `Char`            | `"T"`                      | `"F"`                       |
| `Bool`            | `"true"`                   | `"false"`                   |
| `Int`             | `"1"`                      | `"0"`                       |

See also [Nullary Operators](@ref nullary_operators).

# Examples
```jldoctest
julia> @atomize print_table(p ∧ q; formatters = formatter(Bool))
┌───────┬───────┬───────┐
│ p     │ q     │ p ∧ q │
├───────┼───────┼───────┤
│ true  │ true  │ true  │
│ false │ true  │ false │
├───────┼───────┼───────┤
│ true  │ false │ false │
│ false │ false │ false │
└───────┴───────┴───────┘

julia> @atomize print_table(p ∧ q; formatters = formatter(Int))
┌───┬───┬───────┐
│ p │ q │ p ∧ q │
├───┼───┼───────┤
│ 1 │ 1 │ 1     │
│ 0 │ 1 │ 0     │
├───┼───┼───────┤
│ 1 │ 0 │ 0     │
│ 0 │ 0 │ 0     │
└───┴───┴───────┘
```
"""
formatter

___print_table(backend::Val{:latex}, io, body; vlines = :all, kwargs...) =
    pretty_table(io, body; backend, vlines, kwargs...)
___print_table(backend::Val{:text}, io, body; kwargs...) =
    pretty_table(io, body; backend, kwargs...)

__print_table(
    backend::Union{Val{:text}, Val{:latex}}, io, body;
    body_hlines = collect(0:2:size(body, 1)), kwargs...
) = ___print_table(backend, io, body; body_hlines, kwargs...)
__print_table(backend::Union{Val{:markdown}, Val{:html}}, io, body; kwargs...) =
    pretty_table(io, body; backend, kwargs...)

_print_table(backend, io, t; formatters = formatter(NullaryOperator), kwargs...) =
    __print_table(backend, io, t.body; header = t.header, formatters, kwargs...)

"""
    print_table(::IO = stdout, xs...; kwargs...)

Print a [`TruthTable`](@ref).

The parameters can be a `TruthTable`, iterable of propositions, or sequence of propositions.

Keyword parameters are passed to [`PrettyTables.pretty_table`]
(https://ronisbr.github.io/PrettyTables.jl/stable/lib/library/#PrettyTables.pretty_table-Tuple{Any}).

# Examples
```jldoctest
julia> print_table(TruthTable([⊤]))
┌───┐
│ ⊤ │
├───┤
│ ⊤ │
└───┘

julia> @atomize print_table([p])
┌───┐
│ p │
├───┤
│ ⊤ │
│ ⊥ │
└───┘

julia> @atomize print_table(p ∧ q)
┌───┬───┬───────┐
│ p │ q │ p ∧ q │
├───┼───┼───────┤
│ ⊤ │ ⊤ │ ⊤     │
│ ⊥ │ ⊤ │ ⊥     │
├───┼───┼───────┤
│ ⊤ │ ⊥ │ ⊥     │
│ ⊥ │ ⊥ │ ⊥     │
└───┴───┴───────┘
```
"""
print_table(io::IO, t::TruthTable; backend = Val(:text), alignment = :l, kwargs...) =
    _print_table(backend, io, t; alignment, kwargs...)
print_table(io::IO, ps; kwargs...) = print_table(io, TruthTable(ps); kwargs...)
print_table(io::IO, @nospecialize(ps::Union{Operator, AbstractSyntaxTree}...); kwargs...) = print_table(io, collect(AbstractSyntaxTree, ps); kwargs...)
print_table(@nospecialize(xs...); kwargs...) = print_table(stdout, xs...; kwargs...)

_print_tree(io, p::Union{Operator, AbstractSyntaxTree}) = printnode(io, p)
_print_tree(io, p) = show(io, "text/plain", AbstractSyntaxTree(p))

"""
    print_tree(::IO = stdout, p; kwargs...)

Print a tree diagram of the given proposition.

Keyword parameters are passed to [`AbstractTrees.print_tree`]
(https://github.com/JuliaCollections/AbstractTrees.jl/blob/master/src/printing.jl).

```jldoctest
julia> @atomize print_tree(p ∧ q ∨ ¬s)
∨
├─ ∧
│  ├─ p
│  └─ q
└─ ¬
   └─ s

julia> @atomize print_tree(normalize(∧, p ∧ q ∨ ¬s))
∧
├─ ∨
│  ├─ q
│  └─ ¬
│     └─ s
└─ ∨
   ├─ ¬
   │  └─ s
   └─ p
```
"""
print_tree(io, p; kwargs...) = AbstractTrees.print_tree(_print_tree, io, p; kwargs...)
print_tree(p; kwargs...) = print_tree(stdout, p; kwargs...)

"""
    print_dimacs(::IO = stdout, p)

Print the DIMACS format of `p`.

# Examples
```jldoctest
julia> @atomize print_dimacs(p ∧ q)
p cnf 2 2
1 0
2 0

julia> @atomize print_dimacs(p ↔ q)
p cnf 2 2
1 -2 0
-1 2 0
```
"""
function print_dimacs(io, p)
    clauses, atoms, mapping, qs = prune(p)

    for q in qs
        for clause in first(prune(distribute(q), atoms, mapping))
            push!(clauses, clause)
        end
    end

    PicoSAT.print_dimacs(io, clauses, length(atoms))
end
print_dimacs(p) = print_dimacs(stdout, p)

# `show`

"""
    show(::IO, ::MIME"text/plain", ::Operator)

Print the operator's [`symbol`](@ref Interface.symbol).

# Examples
```jldoctest
julia> show(stdout, "text/plain", ⊤)
⊤

julia> show(stdout, "text/plain", ¬)
¬

julia> show(stdout, "text/plain", ∧)
∧
```
"""
show(io::IO, ::MIME"text/plain", o::Operator) = print(io, symbol(o))

"""
    show(::IO, ::MIME"text/plain", p)

Print the proposition in logical syntax format.

The value of a constant is shown with an `IOContext` whose `:compact` and `:limit`
keys are individually set to `true` if they have not already been set.

# Examples
```jldoctest
julia> @atomize show(stdout, "text/plain", p ∧ q)
p ∧ q

julia> @atomize show(stdout, "text/plain", (p ∨ q) ∧ (r ∨ s))
(p ∨ q) ∧ (r ∨ s)
```
"""
show(io::IO, ::MIME"text/plain", p::AbstractSyntaxTree) =
    _print_proposition(IOContext(io, :root => true, map(key -> key => get(io, key, true), (:compact, :limit))...), p)

"""
    show(::IO, ::MIME"text/plain", ::TruthTable)

Print the [`TruthTable`](@ref)'s default format.

# Examples
```jldoctest
julia> @atomize show(stdout, "text/plain", TruthTable([p ∧ q]))
┌───┬───┬───────┐
│ p │ q │ p ∧ q │
├───┼───┼───────┤
│ ⊤ │ ⊤ │ ⊤     │
│ ⊥ │ ⊤ │ ⊥     │
├───┼───┼───────┤
│ ⊤ │ ⊥ │ ⊥     │
│ ⊥ │ ⊥ │ ⊥     │
└───┴───┴───────┘
```
"""
show(io::IO, ::MIME"text/plain", t::TruthTable) = print_table(io, t; newline_at_end = false)

_show(f, g, io, ps) = parenthesize(io) do
    qs = Stateful(ps)
    for q in qs
        g(io, q)
        isempty(qs) || f(io)
    end
end

"""
    show(::IO, ::Operator)
"""
show(io::IO, o::Operator) = print(io, name(o))

"""
    show(::IO, p)

Print the proposition verbosely.

# Examples
```jldoctest
julia> @atomize show(stdout, p ∧ q)
and(PAndQ.AbstractSyntaxTree(:p), PAndQ.AbstractSyntaxTree(:q))

julia> and(PAndQ.AbstractSyntaxTree(:p), PAndQ.AbstractSyntaxTree(:q))
p ∧ q
```
"""
show(io::IO, p::AbstractSyntaxTree) =
    if p.kind == operator
        show(io, p.value::Operator)
        print(io, "(")
        _show(io -> print(io, ", "), show, io, children(p))
        print(io, ")")
    else
        print(io, AbstractSyntaxTree, "(")
        show(io, p.value)
        print(io, ")")
    end
