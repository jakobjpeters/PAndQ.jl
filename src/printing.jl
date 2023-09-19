
import Base: show
import PrettyTables: pretty_table, _pretty_table
using Base.Docs: HTML
using AbstractTrees: print_tree
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
┌────────┬──────────┬──────────┬──────────────────────────┐
│ p ∧ ¬p │ p        │ q        │ p ⊻ q, (p ⊼ q) ∧ (p ∨ q) │
│ Tree   │ Variable │ Variable │ Tree, Tree               │
├────────┼──────────┼──────────┼──────────────────────────┤
│ ⊥      │ ⊤        │ ⊤        │ ⊥                        │
│ ⊥      │ ⊥        │ ⊤        │ ⊤                        │
├────────┼──────────┼──────────┼──────────────────────────┤
│ ⊥      │ ⊤        │ ⊥        │ ⊤                        │
│ ⊥      │ ⊥        │ ⊥        │ ⊥                        │
└────────┴──────────┴──────────┴──────────────────────────┘
```
"""
struct TruthTable
    header::Vector{Vector{Proposition}}
    body::Matrix{NullaryOperator}

    function TruthTable(ps)
        _atoms = union(map(atoms, ps)...)
        ps = union(_atoms, ps)
        _valuations = valuations(_atoms)
        _interpretations = Iterators.map(p -> collect(interpretations(p, _valuations)), ps)

        truths_interpretations, atoms_interpretations, compounds_interpretations =
            Vector{NullaryOperator}[], Vector{NullaryOperator}[], Vector{NullaryOperator}[]

        grouped_truths = Dict(map(no -> repeat([no], length(_valuations)) => Proposition[], (tautology, contradiction)))
        grouped_atoms = Dict(map(
            p -> collect(interpretations(p, _valuations)) => Proposition[],
            _atoms
        ))
        grouped_compounds = Dict{Vector{NullaryOperator}, Vector{Proposition}}()

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
        body = Vector{NullaryOperator}[]
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
symbol_of(::typeof(identity)) = ""
for lo in (:⊤, :⊥, :¬, :∧, :⊼, :⊽, :∨, :⊻, :↔, :→, :↛, :←, :↚)
    @eval symbol_of(::typeof($lo)) = $(QuoteNode(lo))
end

"""
    merge_string(cell)
"""
merge_string(cell::LatexCell) = cell
merge_string(cell) = join(Iterators.map(
    p -> sprint(
        (io, q) -> show(io, MIME"text/plain"(), q), p
    ), cell
), ", ")

"""
    parenthesize(::IO, x)
"""
parenthesize(io::IO, x) = show(io, MIME"text/plain"(), x)
function parenthesize(io::IO, x::Union{Clause, Tree{<:BinaryOperator}})
    print(io, "(")
    show(io, MIME"text/plain"(), x)
    print(io, ")")
end

# `show`

"""
    show(::IO, ::MIME"text/plain", ::Proposition)

Represent the given [`Proposition`](@ref) as a [propositional formula]
(https://en.wikipedia.org/wiki/Propositional_formula).

# Examples
```jldoctest
julia> @atomize x = p ⊻ q;

julia> show(stdout, MIME"text/plain"(), x)
p ⊻ q

julia> show(stdout, MIME"text/plain"(), Normal(x))
(p ∨ q) ∧ (¬p ∨ ¬q)
```
"""
function show(io::IO, ::MIME"text/plain", p::Constant)
    print(io, "\$(")
    show(io, p.value)
    print(io, ")")
end
show(io::IO, ::MIME"text/plain", p::Variable) = print(io, p.symbol)
function show(io::IO, ::MIME"text/plain", p::Literal{UO}) where UO
    print(io, symbol_of(UO.instance))
    show(io, MIME"text/plain"(), p.atom)
end
show(io::IO, ::MIME"text/plain", p::Tree{NO}) where NO <: NullaryOperator =
    print(io, symbol_of(NO.instance))
show(io::IO, ::MIME"text/plain", p::Tree{typeof(identity)}) =
    show(io, MIME"text/plain"(), only(p.nodes))
function show(io::IO, ::MIME"text/plain", p::Tree{N, <:Atom}) where N <: typeof(¬)
    print(io, symbol_of(N.instance))
    show(io, MIME"text/plain"(), only(p.nodes))
end
function show(io::IO, ::MIME"text/plain", p::Tree{N, <:Tree}) where N <: typeof(¬)
    print(io, symbol_of(N.instance), "(")
    show(io, MIME"text/plain"(), only(p.nodes))
    print(io, ")")
end
function show(io::IO, ::MIME"text/plain", p::Tree{BO}) where BO <: BinaryOperator
    parenthesize(io, first(p.nodes))
    print(io, " ", symbol_of(BO.instance), " ")
    parenthesize(io, last(p.nodes))
end
function show(io::IO, ::MIME"text/plain", p::Union{Clause{AO}, Normal{AO}}) where AO
    ao = AO.instance
    qs = only_field(p)
    isempty(qs) ?
        print(io, symbol_of(only(left_neutrals(ao)))) :
        join(
            io,
            Iterators.map(q -> sprint(parenthesize, q), qs),
            " $(symbol_of(ao)) "
        )
end

"""
    show(io::IO, ::MIME"text/plain", tt::TruthTable)

Equivalent to [`pretty_table(io, tt; alignment = :l, newline_at_end = false)`](@ref pretty_table).
"""
show(io::IO, ::MIME"text/plain", tt::TruthTable) =
    pretty_table(io, tt; alignment = :l, newline_at_end = false)

"""
    show(::IO, ::Proposition)

Represent the given [`Proposition`](@ref) expanded as valid Julia code.

# Examples
```jldoctest
julia> @atomize s = sprint(show, p ∧ q)
"Tree(and, Tree(identity, Variable(:p)), Tree(identity, Variable(:q)))"

julia> @eval \$(Meta.parse(s))
p ∧ q
```
"""
function show(io::IO, p::A) where A <: Atom
    print(io, nameof(A), "(")
    show(io, only_field(p))
    print(io, ")")
end
show(io::IO, p::L) where {UO, L <: Literal{UO}} =
    print(io, nameof(L), "(", UO.instance, ", ", p.atom, ")")
function show(io::IO, p::T) where {LO, T <: Tree{LO}}
    print(io, nameof(T), "(", LO.instance)
    !isempty(p.nodes) && print(io, ", ", sprint((io, node) -> join(io, node, ", "), p.nodes))
    print(io, ")")
end
show(io::IO, p::CN) where {AO, CN <: Union{Clause{AO}, Normal{AO}}} = print(io,
    nameof(CN), "(", AO.instance, ", [", sprint((io, xs) -> join(io, xs, ", "), only_field(p)), "])"
)

for (T, f) in (
    NullaryOperator => symbol_of,
    String => string ∘ nameof,
    Char => v -> v == ⊤ ? 'T' : 'F',
    Bool => Bool,
    Int => Int ∘ Bool
)
    @eval formatter(::Type{$T}) = (v, _, _) -> $f(v)
end

"""
    formatter(t::Type{<:Union{NullaryOperator, String, Char, Bool, Int}})

| t               | formatter(t)(⊤, _, _) | formatter(t)(⊥, _, _) |
| :-------------  | :-------------------- | :-------------------- |
| NullaryOperator | :⊤                    | :⊥                    |
| String          | "tautology"           | "contradiction"       |
| Char            | 'T'                   | 'F'                   |
| Bool            | true                  | false                 |
| Int             | 1                     | 0                     |
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

__pretty_table(backend, io, truth_table; formatters = formatter(NullaryOperator), kwargs...) =
    ___pretty_table(backend, io, truth_table.body; header = (
        map(merge_string, truth_table.header),
        map(p -> merge_string(map(union_all_type, p)), truth_table.header)
    ), formatters, kwargs...)

_pretty_table(io::IO, p::Proposition; kwargs...) =
    pretty_table(io, TruthTable((p,)); kwargs...)
_pretty_table(io::IO, truth_table::TruthTable; kwargs...) =
    pretty_table(io, truth_table; kwargs...)

"""
    pretty_table(
        ::Union{IO, Type{Union{String, HTML}}} = stdout, ::Union{Proposition, TruthTable};
        formatters = formatter(NullaryOperator), kwargs...
    )

See also [`PrettyTables.pretty_table`]
(https://ronisbr.github.io/PrettyTables.jl/stable/lib/library/#PrettyTables.pretty_table-Tuple{Any}),
[`Proposition`](@ref), [`TruthTable`](@ref), and [`formatter`](@ref).

# Examples
```jldoctest
julia> pretty_table(@atomize p ∧ q)
┌──────────┬──────────┬───────┐
│        p │        q │ p ∧ q │
│ Variable │ Variable │  Tree │
├──────────┼──────────┼───────┤
│        ⊤ │        ⊤ │     ⊤ │
│        ⊥ │        ⊤ │     ⊥ │
├──────────┼──────────┼───────┤
│        ⊤ │        ⊥ │     ⊥ │
│        ⊥ │        ⊥ │     ⊥ │
└──────────┴──────────┴───────┘

julia> print(pretty_table(HTML, @atomize p ∧ q).content)
<table>
  <thead>
    <tr class = "header">
      <th style = "text-align: right;">p</th>
      <th style = "text-align: right;">q</th>
      <th style = "text-align: right;">p ∧ q</th>
    </tr>
    <tr class = "subheader headerLastRow">
      <th style = "text-align: right;">Variable</th>
      <th style = "text-align: right;">Variable</th>
      <th style = "text-align: right;">Tree</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td style = "text-align: right;">⊤</td>
      <td style = "text-align: right;">⊤</td>
      <td style = "text-align: right;">⊤</td>
    </tr>
    <tr>
      <td style = "text-align: right;">⊥</td>
      <td style = "text-align: right;">⊤</td>
      <td style = "text-align: right;">⊥</td>
    </tr>
    <tr>
      <td style = "text-align: right;">⊤</td>
      <td style = "text-align: right;">⊥</td>
      <td style = "text-align: right;">⊥</td>
    </tr>
    <tr>
      <td style = "text-align: right;">⊥</td>
      <td style = "text-align: right;">⊥</td>
      <td style = "text-align: right;">⊥</td>
    </tr>
  </tbody>
</table>
```
"""
pretty_table(io::IO, truth_table::TruthTable; backend = Val(:text), kwargs...) =
    __pretty_table(backend, io, truth_table; kwargs...)

"""
    print_tree(::Function, ::Function, ::IO, ::Proposition; kwargs...)
    print_tree(::IO = stdout, ::Proposition; kwargs...)

Prints a tree diagram of the given [`Proposition`](@ref).

See also [`AbstractTrees.print_tree`]
(https://github.com/JuliaCollections/AbstractTrees.jl/blob/master/src/printing.jl).

```jldoctest
julia> @atomize r = p ∧ ¬q ⊻ s
(p ∧ ¬q) ⊻ s

julia> print_tree(r)
⊻
├─ ∧
│  ├─ I
│  │  └─ p
│  └─ ¬
│     └─ q
└─ I
   └─ s

julia> print_tree(Normal(r))
∧
├─ ∨
│  ├─ I
│  │  └─ p
│  └─ I
│     └─ s
├─ ∨
│  ├─ ¬
│  │  └─ q
│  └─ I
│     └─ s
└─ ∨
   ├─ ¬
   │  └─ p
   ├─ I
   │  └─ q
   └─ ¬
      └─ s
```
"""
print_tree
