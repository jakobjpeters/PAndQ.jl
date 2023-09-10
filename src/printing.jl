
import Base: show
using AbstractTrees: print_tree
using PrettyTables: LatexCell, pretty_table
using REPL: symbol_latex, symbols_latex
using Markdown: MD, Table

#=
    truth_table specification

each row is an interpretation
each column maps to proposition
the first header is the `repr` of that proposition
the second header is the type of that proposition
logically equivalent propositions are put in the same column, seperated by a comma
the order of the columns is determined first by type
    1) truth, 2) atom, 3) any,
    and then by the order it was entered/found
truths only generate a single row, and no propositions
=#
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
        # ToDo: write docstring - define behavior
        # ToDo: write tests

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
    operator_to_symbol(::LogicalOperator)

Return the Unicode character that is an alias for the given [`LogicalOperator`](@ref).

# Examples
```jldoctest
julia> PAndQ.operator_to_symbol(⊤)
"⊤"

julia> PAndQ.operator_to_symbol(¬)
"¬"

julia> PAndQ.operator_to_symbol(∧)
"∧"
```
"""
operator_to_symbol(::typeof(identity)) = ""
for operator_symbol in (:⊤, :⊥, :¬, :∧, :⊼, :⊽, :∨, :⊻, :↔, :→, :↛, :←, :↚)
    @eval operator_to_symbol(::typeof($operator_symbol)) = $(string(operator_symbol))
end

"""
    merge_string(cell)
"""
merge_string(cell) = join(Iterators.map(
    p -> sprint(
        (io, q) -> show(io, MIME"text/plain"(), q), p
    ), cell
), ", ")

"""
    format_letter(::NullaryOperator)

# Examples
```jldoctest
julia> PAndQ.format_letter(tautology)
:T

julia> PAndQ.format_letter(contradiction)
:F
```
"""
format_letter(::typeof(⊤)) = :T
format_letter(::typeof(⊥)) = :F

"""
    format_latex(x)
"""
format_latex(x) = LatexCell(print_latex(String, x))

const _format_body = Dict(
    :truth => operator_to_symbol,
    :text => nameof,
    :letter => format_letter,
    :bool => Bool,
    :bit =>  Int ∘ Bool,
    :latex => format_latex ∘ operator_to_symbol
)

"""
    format_body
"""
format_body(format, cell) = _format_body[format](cell)

"""
    _newline(::Bool)
"""
_newline(newline) = newline ? "\n" : ""

"""
    print_string
"""
function print_string(f, args...; kwargs...)
    buffer = IOBuffer()
    f(buffer, args...; kwargs...)
    String(take!(buffer))
end

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
    print(io, operator_to_symbol(UO.instance))
    show(io, MIME"text/plain"(), p.atom)
end
show(io::IO, ::MIME"text/plain", p::Tree{NO}) where NO <: NullaryOperator =
    print(io, operator_to_symbol(NO.instance))
show(io::IO, ::MIME"text/plain", p::Tree{typeof(identity)}) =
    show(io, MIME"text/plain"(), only(p.nodes))
function show(io::IO, ::MIME"text/plain", p::Tree{N, <:Atom}) where N <: typeof(¬)
    print(io, operator_to_symbol(N.instance))
    show(io, MIME"text/plain"(), only(p.nodes))
end
function show(io::IO, ::MIME"text/plain", p::Tree{N, <:Tree}) where N <: typeof(¬)
    print(io, operator_to_symbol(N.instance), "(")
    show(io, MIME"text/plain"(), only(p.nodes))
    print(io, ")")
end
function show(io::IO, ::MIME"text/plain", p::Tree{BO}) where BO <: BinaryOperator
    parenthesize(io, first(p.nodes))
    print(io, " ", operator_to_symbol(BO.instance), " ")
    parenthesize(io, last(p.nodes))
end
function show(io::IO, ::MIME"text/plain", p::Union{Clause{AO}, Normal{AO}}) where AO
    ao = AO.instance
    qs = only_field(p)
    isempty(qs) ?
        print(io, operator_to_symbol(only(left_neutrals(ao)))) :
        join(
            io,
            Iterators.map(q -> sprint(parenthesize, q), qs),
            " " * operator_to_symbol(ao) * " "
        )
end
show(io::IO, ::MIME"text/plain", truth_table::TruthTable) =
    print_truth_table(io, truth_table)
show(io::IO, ::MIME"text/latex", truth_table::TruthTable) =
    print_latex(io, truth_table)

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

# Works In Progress

"""
    print_tree(::Proposition)

Prints a tree diagram of the given [`Proposition`](@ref).

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

____print_truth_table(backend::Val{:latex}, io, body; vlines = :all, kwargs...) =
    pretty_table(io, body; backend, vlines, kwargs...)
____print_truth_table(backend::Val{:text}, io, body; crop = :none, newline, kwargs...) =
    pretty_table(io, body; backend, crop, newline_at_end = newline, kwargs...)

___print_truth_table(
    backend::Union{Val{:text}, Val{:latex}}, io, body;
    body_hlines = collect(0:2:size(body, 1)), kwargs...
) = ____print_truth_table(backend, io, body; body_hlines, kwargs...)
___print_truth_table(backend::Val{:html}, io, body; kwargs...) =
    pretty_table(io, body; backend, kwargs...)

function __print_truth_table(
    backend, io, truth_table;
    sub_header = true, numbered_rows = false, format = :truth, alignment = :l,
    kwargs...
)
    header = map(
        cell -> merge_string(format == :latex ? format_latex(cell) : cell),
        truth_table.header
    )
    if sub_header
        header = (header, map(p -> merge_string(map(union_all_type, p)), truth_table.header))
    end

    body = map(cell -> format_body(format, cell), truth_table.body)

    if numbered_rows
        header = sub_header ? (map(vcat, ["#", ""], header)...,) : vcat("#", header)
        body = hcat(map(string, 1:size(body, 1)), body)
    end

    ___print_truth_table(backend, io, body; header, alignment, kwargs...)
end

_print_truth_table(backend::Val, io, truth_table; kwargs...) =
    __print_truth_table(backend, io, truth_table; kwargs...)
_print_truth_table(backend::Val{:latex}, io, truth_table; format = :latex, kwargs...) =
    __print_truth_table(backend, io, truth_table; format, kwargs...)
_print_truth_table(backend::Val{:text}, io, truth_table; newline = false, kwargs...) =
    __print_truth_table(backend, io, truth_table; newline, kwargs...)

"""
    print_truth_table([io::Union{IO, String}], x, backend = :text, kwargs...)

# Examples
"""
print_truth_table(io::IO, truth_table::TruthTable; backend = :text, kwargs...) =
    _print_truth_table(Val(backend), io, truth_table; kwargs...)
print_truth_table(io::IO, x; kwargs...) =
    print_truth_table(io, TruthTable(x); kwargs...)
print_truth_table(x; kwargs...) =
    print_truth_table(stdout, x; kwargs...)

"""
    print_latex([io::Union{IO, String} = stdout], x, delimeter = "\\(" => "\\)")

Return a string representation of `x` enclosed by `delimeter`,
replacing each symbol with it's respective command.

# Examples
```jldoctest
julia> @atomize s = print_latex(String, p ∧ q)
"\\\\(p \\\\wedge q\\\\)"

julia> println(s)
\\(p \\wedge q\\)
```
"""
function print_latex(io::IO, x::String; newline = false, delimeter = "\\(" => "\\)")
    isempty(symbols_latex) && symbol_latex("")

    latex = join((
        first(delimeter),
        rstrip(mapreduce(
            c -> c == ' ' ? "" : get(symbols_latex, string(c), c) * " ",
            *,
            x
        )),
        last(delimeter)
    ))

    print(io, latex, _newline(newline))
end
print_latex(io::IO, x::TruthTable; kwargs...) =
    print_truth_table(io, x; backend = :latex, kwargs...)
print_latex(io::IO, x; kwargs...) = print_latex(io,
    sprint((io, y) -> show(io, MIME"text/plain"(), y), x);
kwargs...)
print_latex(x; kwargs...) = print_latex(stdout, x; kwargs...)

"""
    print_markdown

# Examples
"""
print_markdown(::Type{MD}, p) = MD(sprint((io, x) -> show(io, MIME"text/plain"(), x), p))
print_markdown(::Type{MD}, truth_table::TruthTable; format = :truth, alignment = :l) = MD(Table(
    [
        map(merge_string, truth_table.header),
        eachrow(map(no -> format_body(format, no), truth_table.body))...
    ],
    repeat([alignment], length(truth_table.header))
))
print_markdown(io::IO, x; newline = false, kwargs...) = print(io,
    string(print_markdown(MD, x; kwargs...))[begin:end - 1],
    _newline(newline)
)
print_markdown(x; kwargs...) = print_markdown(stdout, x; kwargs...)

for f in (:latex, :truth_table, :markdown)
    print_f, println_f = map(s -> Symbol("print", s, "_", f), ("", "ln"))

    @eval $print_f(::Type{String}, args...; kwargs...) = print_string($print_f, args...; kwargs...)
    @eval begin
        print_f = $print_f
        println_f = $(string(println_f))
        """
            $println_f(args...; kwargs...)

        Equivalent to [`$print_f(args...; kwargs..., newline = true)`](@ref $print_f).

        # Examples
        """
        $println_f(args...; kwargs...) = $print_f(args...; kwargs..., newline = true)
    end
end
