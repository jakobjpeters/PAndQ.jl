
import Base: show, Stateful
import PrettyTables: pretty_table, _pretty_table
import AbstractTrees: print_tree
using Base.Docs: HTML
using AbstractTrees: print_child_key
using PrettyTables: LatexCell

"""
    TruthTable(ps)

Construct a [truth table](https://en.wikipedia.org/wiki/Truth_table)
for the given [`Proposition`](@ref)s and [`LogicalOperator`](@ref)s.

The `header` is a vector containing vectors of logically equivalent propositions.
The `sub_header` corresponds to the `header`, but contains each proposition's `UnionAll` type.
The `body` is a matrix where the rows contain [`interpretations`](@ref) of each proposition in the given column.

See also [`tautology`](@ref) and [`contradiction`](@ref).

# Examples
```jldoctest
julia> TruthTable([Tree(⊤)])
┌──────┐
│ ⊤    │
│ Tree │
├──────┤
│ ⊤    │
└──────┘

julia> @atomize TruthTable([¬p])
┌──────────┬─────────┐
│ p        │ ¬p      │
│ Variable │ Literal │
├──────────┼─────────┤
│ ⊤        │ ⊥       │
│ ⊥        │ ⊤       │
└──────────┴─────────┘

julia> @atomize TruthTable([p ∧ ¬p, p ⊻ q, ¬(p ∧ q) ∧ (p ∨ q)])
┌────────┬──────────┬──────────┬───────────────────────────┐
│ p ∧ ¬p │ p        │ q        │ p ⊻ q, ¬(p ∧ q) ∧ (p ∨ q) │
│ Tree   │ Variable │ Variable │ Tree, Tree                │
├────────┼──────────┼──────────┼───────────────────────────┤
│ ⊥      │ ⊤        │ ⊤        │ ⊥                         │
│ ⊥      │ ⊥        │ ⊤        │ ⊤                         │
├────────┼──────────┼──────────┼───────────────────────────┤
│ ⊥      │ ⊤        │ ⊥        │ ⊤                         │
│ ⊥      │ ⊥        │ ⊥        │ ⊥                         │
└────────┴──────────┴──────────┴───────────────────────────┘
```
"""
struct TruthTable
    header::Vector{Vector{Proposition}}
    body::Matrix{Bool}

    function TruthTable(ps)
        _atoms = union(map(atoms, ps)...)
        ps = union(_atoms, ps)
        _valuations = valuations(_atoms)
        _interpretations = Iterators.map(p -> vec(collect(interpretations(p, _valuations))), ps)

        truths_interpretations, atoms_interpretations, compounds_interpretations =
            Vector{Bool}[], Vector{Bool}[], Vector{Bool}[]

        grouped_truths = Dict(map(no -> repeat([no], length(_valuations)) => Proposition[], (true, false)))
        grouped_atoms = Dict(map(
            p -> collect(interpretations(p, _valuations)) => Proposition[],
            _atoms
        ))
        grouped_compounds = Dict{Vector{Bool}, Vector{Proposition}}()

        for (p, interpretation) in zip(ps, _interpretations)
            _union! = (key, group) -> begin
                union!(key, [interpretation])
                union!(get!(group, interpretation, Proposition[]), [p])
            end
            if interpretation in keys(grouped_truths) _union!(truths_interpretations, grouped_truths)
            elseif interpretation in keys(grouped_atoms) _union!(atoms_interpretations, grouped_atoms)
            else _union!(compounds_interpretations, grouped_compounds)
            end
        end

        header = Vector{Proposition}[]
        body = Vector{Bool}[]
        for (_interpretations, group) in (
            truths_interpretations => grouped_truths,
            atoms_interpretations => grouped_atoms,
            compounds_interpretations => grouped_compounds
        )
            for interpretation in _interpretations
                xs = get(group, interpretation, Proposition[])
                push!(header, xs)
                push!(body, interpretation)
            end
        end

        new(header, reduce(hcat, body))
    end
end

# Internals

"""
    alias_of(::LogicalOperator)
"""
alias_of(lo::LogicalOperator) = nameof(lo)
for lo in (:not, :and, :or)
    @eval alias_of(::typeof($lo)) = $(QuoteNode(lo))
end

"""
    symbol_of(::LogicalOperator)

Return the Unicode character that is an alias for the given [`LogicalOperator`](@ref).

# Examples
```jldoctest
julia> PAndQ.symbol_of(⊤)
:⊤

julia> PAndQ.symbol_of(¬)
:¬

julia> PAndQ.symbol_of(∧)
:∧
```
"""
symbol_of(::typeof(identity)) = Symbol("")
for lo in (:⊤, :⊥, :¬, :∧, :⊼, :⊽, :∨, :⊻, :↔, :→, :↛, :←, :↚)
    @eval symbol_of(::typeof($lo)) = $(QuoteNode(lo))
end

"""
    merge_string(cell)
"""
merge_string(cell::LatexCell) = cell
merge_string(cell) =
    join(Iterators.map(p -> sprint(show, MIME"text/plain"(), p), cell), ", ")

"""
    parenthesize(::IO, x)
"""
parenthesize(io, x) = show(io, MIME"text/plain"(), x)
function parenthesize(io::IO, x::Union{Clause, <:Tree{<:BinaryOperator}})
    print(io, "(")
    show(io, MIME"text/plain"(), x)
    print(io, ")")
end

"""
    print_node(io, p)
"""
print_node(io, ::Compound{typeof(identity)}) = print(io, "𝒾")
print_node(io, p) = printnode(io, p)

"""
    show_atom
"""
show_atom(io, p::Constant) = show(io, p.value)
show_atom(io, p::Variable) = show(io, p.symbol)

# `show`

"""
    show(::IO, ::MIME"text/plain", ::Proposition)

Represent the given [`Proposition`](@ref) as a [propositional formula]
(https://en.wikipedia.org/wiki/Propositional_formula).

# Examples
```jldoctest
julia> @atomize show(stdout, MIME"text/plain"(), p ⊻ q)
p ⊻ q

julia> @atomize show(stdout, MIME"text/plain"(), Normal(p ⊻ q))
(p ∨ q) ∧ (¬p ∨ ¬q)
```
"""
function show(io::IO, ::MIME"text/plain", p::Constant)
    print(io, "\$(")
    show(io, p.value)
    print(io, ")")
end
show(io::IO, ::MIME"text/plain", p::Variable) = print(io, p.symbol)
function show(io::IO, ::MIME"text/plain", p::Literal)
    printnode(io, p)
    printnode(io, p.atom)
end
function show(io::IO, ::MIME"text/plain", p::Compound{<:UnaryOperator})
    printnode(io, p)
    parenthesize(io, child(p))
end
function show(io::IO, ::MIME"text/plain", p::Compound)
    _children = Stateful(children(p))
    isempty(_children) ?
        printnode(io, p) :
        for child in _children
            parenthesize(io, child)
            if !isempty(_children)
                print(io, " ")
                printnode(io, p)
                print(io, " ")
            end
        end
end

"""
    show(::IO, ::MIME"text/plain", ::TruthTable)

# Examples
```julia
julia> @atomize show(stdout, MIME"text/plain"(), TruthTable([p ∧ q]))
```
"""
show(io::IO, ::MIME"text/plain", tt::TruthTable) =
    pretty_table(io, tt; newline_at_end = false)

"""
    show(::IO, ::Proposition)

Represent the given [`Proposition`](@ref) expanded as valid Julia code.

# Examples
```jldoctest
julia> @atomize s = sprint(show, p ∧ q)
"Tree(and, Tree(identity, Variable(:p)), Tree(identity, Variable(:q)))"

julia> eval(Meta.parse(s))
p ∧ q
```
"""
function show(io::IO, p::A) where A <: Atom
    print(io, nameof(A), "(")
    show_atom(io, p)
    print(io, ")")
end
show(io::IO, p::L) where L <: Literal =
    print(io, nameof(L), "(", alias_of(nodevalue(p)), ", ", p.atom, ")")
function show(io::IO, p::C) where C <: Compound
    print(io, nameof(C), "(", alias_of(nodevalue(p)))

    _children = Stateful(children(p))
    if !isempty(_children)
        print(io, ", ")
        p isa Union{Clause, Normal} && print(io, "[")

        for node in _children
            show(io, node)
            !isempty(_children) && print(io, ", ")
        end

        p isa Union{Clause, Normal} && print(io, "]")
    end

    print(io, ")")
end

for (T, f) in (
    NullaryOperator => v -> v ? "⊤" : "⊥",
    String => v -> alias_of(v ? ⊤ : ⊥),
    Char => v -> v == ⊤ ? "T" : "F",
    Bool => identity,
    Int => Int
)
    @eval formatter(::Type{$T}) = (v, _, _) -> string($f(v))
end

"""
    formatter(t::Type{<:Union{PAndQ.NullaryOperator, String, Char, Bool, Int}})

| `t`                     | `formatter(t)(⊤, _, _)` | `formatter(t)(⊥, _, _)` |
| :---------------------- | :---------------------- | :---------------------- |
| `PAndQ.NullaryOperator` | `"⊤"`                   | `"⊥"`                   |
| `String`                | `"tautology"`           | `"contradiction"`       |
| `Char`                  | `"T"`                   | `"F"`                   |
| `Bool`                  | `"true"`                | `"false"`               |
| `Int`                   | `"1"`                   | `"0"`                   |
"""
formatter

____pretty_table(backend::Val{:latex}, io, body; vlines = :all, kwargs...) =
    pretty_table(io, body; backend, vlines, kwargs...)
____pretty_table(backend::Val{:text}, io, body; crop = :none, kwargs...) =
    pretty_table(io, body; backend, crop, kwargs...)

___pretty_table(
    backend::Union{Val{:text}, Val{:latex}}, io, body;
    body_hlines = collect(0:2:size(body, 1)), kwargs...
) = ____pretty_table(backend, io, body; body_hlines, kwargs...)
___pretty_table(backend::Val{:html}, io, body; kwargs...) =
    pretty_table(io, body; backend, kwargs...)

__pretty_table(backend, io, tt; formatters = formatter(NullaryOperator), kwargs...) =
    ___pretty_table(backend, io, tt.body; header = (
        map(merge_string, tt.header),
        map(p -> merge_string(map(union_all_type, p)), tt.header)
    ), formatters, kwargs...)

_pretty_table(io::IO, p::Proposition; kwargs...) =
    pretty_table(io, TruthTable((p,)); kwargs...)
_pretty_table(io::IO, tt::TruthTable; kwargs...) =
    pretty_table(io, tt; kwargs...)

"""
    pretty_table(
        ::Union{IO, Type{Union{String, Docs.HTML}}} = stdout, ::Union{Proposition, TruthTable};
        formatters = formatter(PAndQ.NullaryOperator), kwargs...
    )

See also [`PrettyTables.pretty_table`]
(https://ronisbr.github.io/PrettyTables.jl/stable/lib/library/#PrettyTables.pretty_table-Tuple{Any}),
[`Proposition`](@ref), [`TruthTable`](@ref), and [`formatter`](@ref).

# Examples
```jldoctest
julia> pretty_table(@atomize p ∧ q)
┌──────────┬──────────┬───────┐
│ p        │ q        │ p ∧ q │
│ Variable │ Variable │ Tree  │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊤        │ ⊤     │
│ ⊥        │ ⊤        │ ⊥     │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊥        │ ⊥     │
│ ⊥        │ ⊥        │ ⊥     │
└──────────┴──────────┴───────┘

julia> print(pretty_table(Docs.HTML, @atomize p ∧ q).content)
<table>
  <thead>
    <tr class = "header">
      <th style = "text-align: left;">p</th>
      <th style = "text-align: left;">q</th>
      <th style = "text-align: left;">p ∧ q</th>
    </tr>
    <tr class = "subheader headerLastRow">
      <th style = "text-align: left;">Variable</th>
      <th style = "text-align: left;">Variable</th>
      <th style = "text-align: left;">Tree</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td style = "text-align: left;">⊤</td>
      <td style = "text-align: left;">⊤</td>
      <td style = "text-align: left;">⊤</td>
    </tr>
    <tr>
      <td style = "text-align: left;">⊥</td>
      <td style = "text-align: left;">⊤</td>
      <td style = "text-align: left;">⊥</td>
    </tr>
    <tr>
      <td style = "text-align: left;">⊤</td>
      <td style = "text-align: left;">⊥</td>
      <td style = "text-align: left;">⊥</td>
    </tr>
    <tr>
      <td style = "text-align: left;">⊥</td>
      <td style = "text-align: left;">⊥</td>
      <td style = "text-align: left;">⊥</td>
    </tr>
  </tbody>
</table>
```
"""
pretty_table(io::IO, tt::TruthTable; backend = Val(:text), alignment = :l, kwargs...) =
    __pretty_table(backend, io, tt; alignment, kwargs...)

"""
    print_tree(::IO = stdout, ::Proposition; kwargs...)

Prints a tree diagram of the given [`Proposition`](@ref).

!!! note
    Instances of [`Compound{typeof(identity)}`](@ref Compound) are represented as `𝒾`.

See also [`AbstractTrees.print_tree`]
(https://github.com/JuliaCollections/AbstractTrees.jl/blob/master/src/printing.jl).

```jldoctest
julia> @atomize print_tree(p ∧ ¬q ⊻ s)
⊻
├─ ∧
│  ├─ 𝒾
│  │  └─ p
│  └─ ¬
│     └─ q
└─ 𝒾
   └─ s

julia> @atomize print_tree(Normal(p ∧ ¬q ⊻ s))
∧
├─ ∨
│  ├─ 𝒾
│  │  └─ p
│  └─ 𝒾
│     └─ s
├─ ∨
│  ├─ ¬
│  │  └─ q
│  └─ 𝒾
│     └─ s
└─ ∨
   ├─ ¬
   │  └─ p
   ├─ 𝒾
   │  └─ q
   └─ ¬
      └─ s
```
"""
print_tree(io::IO, p::Proposition; kwargs...) =
    print_tree(print_node, print_child_key, io, p; kwargs...)
