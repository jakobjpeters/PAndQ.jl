
import Base: show
import AbstractTrees # print_tree
import AbstractTrees: children, nodevalue, printnode

using PrettyTables
using REPL: symbol_latex, symbols_latex
using Markdown: MD, Table

# TODO: print_html, print_typst

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

operator_to_proposition(x::NullaryOperator) = x |> Clause
operator_to_proposition(x::UnaryOperator) = :_ |> Atom |> x
operator_to_proposition(x::BinaryOperator) = x(Atom(:_), Atom(:__))
operator_to_proposition(p::Proposition) = p

"""
    TruthTable(::AbstractArray)
    TruthTable(ps...)

Construct a [truth table](https://en.wikipedia.org/wiki/Truth_table)
for the given [`Proposition`](@ref)s and [`BinaryOperator`](@ref)s.

The `header` is a vector containing vectors of logically equivalent propositions.
The `sub_header` corresponds to the `header`, but contains each proposition's `UnionAll` type.
The `body` is a matrix where the rows contain [`interpretations`](@ref)
and the columns correspond to elements in the `header` and `sub_header`.

Logically equivalent propositions will be grouped in the same column, seperated by a comma.

See also [`tautology`](@ref) and [`contradiction`](@ref).

# Examples
```jldoctest
julia> @p TruthTable(p ∧ ¬p, p ∧ q)
┌────────┬──────┬──────┬───────┐
│ p ∧ ¬p │ p    │ q    │ p ∧ q │
│ Tree   │ Atom │ Atom │ Tree  │
├────────┼──────┼──────┼───────┤
│ ⊥      │ ⊤    │ ⊤    │ ⊤     │
│ ⊥      │ ⊥    │ ⊤    │ ⊥     │
├────────┼──────┼──────┼───────┤
│ ⊥      │ ⊤    │ ⊥    │ ⊥     │
│ ⊥      │ ⊥    │ ⊥    │ ⊥     │
└────────┴──────┴──────┴───────┘

julia> TruthTable([⊻, imply])
┌──────┬──────┬────────┬────────┐
│ _    │ __   │ _ ⊻ __ │ _ → __ │
│ Atom │ Atom │ Tree   │ Tree   │
├──────┼──────┼────────┼────────┤
│ ⊤    │ ⊤    │ ⊥      │ ⊤      │
│ ⊥    │ ⊤    │ ⊤      │ ⊤      │
├──────┼──────┼────────┼────────┤
│ ⊤    │ ⊥    │ ⊤      │ ⊥      │
│ ⊥    │ ⊥    │ ⊥      │ ⊤      │
└──────┴──────┴────────┴────────┘
```
"""
struct TruthTable
    header::Vector{Vector{Proposition}}
    sub_header::Vector{Vector{UnionAll}}
    body::Matrix{Function}

    TruthTable(ps::AbstractArray) = begin
        # ToDo: write docstring - define behavior
        # ToDo: write tests
        # TODO: make header support operators (`⊤; NullaryOperator`, `⊻; BinaryOperator`)

        ps = Iterators.map(operator_to_proposition, ps)
        _atoms = mapreduce(atoms, union, ps)
        ps = union(_atoms, ps)
        _valuations = _atoms |> valuations
        _interpretations = Iterators.map(p -> interpretations(p, _valuations) |> collect, ps)

        truths_interpretations, atoms_interpretations, compounds_interpretations =
            Vector{Function}[], Vector{Function}[], Vector{Function}[]

        group = xs -> map(xs) do x
            interpretations(x, _valuations) |> collect => Proposition[]
        end |> Dict
        grouped_truths, grouped_atoms = map(group, ((tautology, contradiction), _atoms))
        grouped_compounds = Dict{Vector{Function}, Vector{Proposition}}()

        foreach(zip(ps, _interpretations)) do (p, interpretation)
            _union! = (key, group) -> begin
                union!(key, [interpretation])
                union!(get!(group, interpretation, Proposition[]), [p])
            end
            if interpretation in grouped_truths |> keys _union!(truths_interpretations, grouped_truths)
            elseif interpretation in grouped_atoms |> keys _union!(atoms_interpretations, grouped_atoms)
            else _union!(compounds_interpretations, grouped_compounds)
            end
        end

        header = Vector{Proposition}[]
        sub_header = Vector{UnionAll}[]
        body = Vector{Function}[]
        foreach((
            truths_interpretations => grouped_truths,
            atoms_interpretations => grouped_atoms,
            compounds_interpretations => grouped_compounds
        )) do (interpretations, group)
            foreach(interpretations) do interpretation
                xs = get(group, interpretation, Proposition[])
                push!(header, xs)
                push!(sub_header, map(x -> getfield(Main, x |> typeof |> nameof), xs))
                push!(body, interpretation)
            end
        end

        new(header, sub_header, reduce(hcat, body))
    end
end
TruthTable(ps...) = ps |> collect |> TruthTable

operator_to_symbol(::typeof(identity)) = ""
foreach((:⊤, :⊥, :¬, :∧, :⊼, :⊽, :∨, :⊻, :↔, :→, :↛, :←, :↚)) do operator_symbol
    @eval operator_to_symbol(::typeof($operator_symbol)) = $(operator_symbol |> string)
end

letter(::typeof(tautology)) = "T"
letter(::typeof(contradiction)) = "F"

format_latex(x) = print_latex(String, x) |> LatexCell

format_head(format, cell) = format == :latex ? format_latex(cell) : cell

const _format_body = Dict(
    :truth => string ∘ operator_to_symbol,
    :text => string ∘ nameof,
    :letter => letter,
    :bool => string ∘ Bool,
    :bit =>  string ∘ Int ∘ Bool,
    :latex => format_latex ∘ operator_to_symbol
)

format_body(format, cell) = cell |> _format_body[format]

____print_truth_table(backend::Val{:latex}, io, body; vlines = :all, kwargs...) =
    pretty_table(io, body; backend, vlines, kwargs...)
____print_truth_table(backend::Val{:text}, io, body; crop = :none, newline, kwargs...) =
    pretty_table(io, body; backend, crop, newline_at_end = newline, kwargs...)

___print_truth_table(
    backend::Union{Val{:text}, Val{:latex}}, io, body;
    body_hlines = 0:2:size(body, 1) |> collect, kwargs...
) = ____print_truth_table(backend, io, body; body_hlines, kwargs...)
___print_truth_table(backend::Val{:html}, io, body; kwargs...) =
    pretty_table(io, body; backend, kwargs...)

merge_string(xs) = join(xs, ", ")

__print_truth_table(
    backend, io, truth_table;
    sub_header = true, numbered_rows = false, format = :truth, alignment = :l,
    kwargs...
) = begin
    header = map(truth_table.header) do p
        format_head(format, p) |> merge_string
    end
    sub_header && (header = (header, map(merge_string, truth_table.sub_header)))

    body = map(truth_table.body) do cell
        format_body(format, cell)
    end

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
    _print_truth_table(backend |> Val, io, truth_table; kwargs...)
print_truth_table(io::IO, x; kwargs...) =
    print_truth_table(io, x |> TruthTable; kwargs...)
print_truth_table(x; kwargs...) =
    print_truth_table(stdout, x; kwargs...)

_newline(newline, io) = newline ? io |> println : nothing

children(p::Tree) = p.nodes
children(p::Tree{typeof(identity)}) = ()

nodevalue(p::Tree{LO}) where LO = LO.instance
nodevalue(p::Tree{typeof(identity)}) = p

printnode(io::IO, node::Tree{typeof(identity)}) = print(io, node |> nodevalue)
printnode(io::IO, node::Tree{LO}) where LO = print(io, LO.instance |> operator_to_symbol)

"""
    print_tree([io::Union{IO, String} = stdout], p; max_depth = typemax(Int64), newline = false, kwargs...)

Prints a tree diagram of `p`.

If `p` isn't a [`Tree`](@ref), it will be converted to one.
The optional argument `max_depth` will truncate sub-trees at that depth.

```jldoctest
julia> @p print_tree(p ⊻ q)
⊻
├─ p
└─ q

julia> @p print_tree((p ∧ ¬q) ∨ (¬p ∧ q))
∨
├─ ∧
│  ├─ p
│  └─ ¬
│     └─ q
└─ ∧
   ├─ ¬
   │  └─ p
   └─ q
```
"""
print_tree(io::IO, p::Tree; max_depth = typemax(Int), newline = false, kwargs...) = begin
    print(io, print_string(AbstractTrees.print_tree, p; maxdepth = max_depth, kwargs...) |> rstrip)
    _newline(newline, io)
end
print_tree(io::IO, p; kwargs...) = print_tree(io, p |> Tree; kwargs...)
print_tree(p; kwargs...) = print_tree(stdout, p; kwargs...)

"""
    print_latex([io::Union{IO, String} = stdout], x, delimeter = "\\(" => "\\)")

Return a string representation of `x` enclosed by `delimeter`,
replacing each symbol with it's respective command.

# Examples
```jldoctest
julia> @p s = print_latex(String, p ∧ q)
"\\\\(p \\\\wedge q\\\\)"

julia> println(s)
\\(p \\wedge q\\)
```
"""
print_latex(io::IO, x::String; newline = false, delimeter = "\\(" => "\\)") = begin
    # populates `symbols_latex`
    symbols_latex |> isempty && "∧" |> symbol_latex

    latex = join([
        delimeter |> first,
        mapreduce(*, x) do c
            c == ' ' ? "" : get(symbols_latex, c |> string, c) * " "
        end |> rstrip,
        delimeter |> last
    ])

    print(io, latex)
    _newline(newline, io)
end
print_latex(io::IO, x::TruthTable; kwargs...) =
    print_truth_table(io, x; backend = :latex, kwargs...)
print_latex(io::IO, x; kwargs...) = print_latex(io, x |> string; kwargs...)
print_latex(x; kwargs...) = print_latex(stdout, x; kwargs...)

"""
    print_markdown

# Examples
"""
print_markdown(::Type{MD}, p) = p |> string |> MD
print_markdown(::Type{MD}, truth_table::TruthTable; format = :truth, alignment = :l) = Table(
    [
        map(merge_string, truth_table.header),
        eachrow(map(no -> format_body(format, no), truth_table.body))...
    ],
    repeat([alignment], truth_table.header |> length)
) |> MD
print_markdown(io::IO, x; newline = false, kwargs...) = begin
    print(io, string(print_markdown(MD, x; kwargs...))[begin:end - 1])
    _newline(newline, io)
end
print_markdown(x; kwargs...) = print_markdown(stdout, x; kwargs...)

print_string(f, args...; kwargs...) = begin
    buffer = IOBuffer()
    f(buffer, args...; kwargs...)
    buffer |> take! |> String
end

foreach([:tree, :latex, :truth_table, :markdown]) do f
    print_f, println_f = map(s -> Symbol("print", s, "_", f), ("", "ln"))

    @eval $print_f(::Type{String}, args...; kwargs...) = print_string($print_f, args...; kwargs...)
    @eval begin
        print_f = $print_f
        println_f = $(println_f |> string)
        """
            $println_f(args...; kwargs...)

        Equivalent to [`$print_f(args...; kwargs..., newline = true)`](@ref $print_f).

        # Examples
        """
        $println_f(args...; kwargs...) = $print_f(args...; kwargs..., newline = true)
    end
end

first_field(p) = getfield(p, 1)

parenthesize(p) = p |> string
parenthesize(p::Union{Clause, Tree{<:BinaryOperator}}) = "(" * string(p) * ")"

"""
    show
"""
show(io::IO, p::Atom) = show(io, p.statement)
show(io::IO, p::Atom{Symbol}) = print(io, p.statement)
show(io::IO, p::Literal{UO}) where UO =
    print(io, UO.instance |> operator_to_symbol, p.atom)
show(io::IO, p::Tree{NO, <:Tuple{}}) where NO =
    print(io, NO.instance |> operator_to_symbol)
show(io::IO, p::Tree{UO, <:Tuple{Atom}}) where UO = 
    print(io, UO.instance |> operator_to_symbol, p.nodes |> only)
show(io::IO, p::Tree{UO, <:Tuple{Tree}}) where UO =
    print(io, UO.instance |> operator_to_symbol, "(", p.nodes |> only, ")")
show(io::IO, p::Tree{BO, <:NTuple{2, Tree}}) where BO = join(io, [
    p.nodes |> first |> parenthesize,
    BO.instance |> operator_to_symbol,
    p.nodes |> last |> parenthesize,
], " ")
show(io::IO, p::Union{Clause{AO}, Normal{AO}}) where AO = begin
    ao = AO.instance
    qs = p |> first_field |> Iterators.Stateful
    qs |> isempty ?
        print(io, ao |> left_neutrals |> only |> operator_to_symbol) :
        join(io, Iterators.map(parenthesize, qs), " " * operator_to_symbol(ao) * " ")
end
show(io::IO, ::MIME"text/plain", p::P) where P <: Proposition =
    print(io, P |> nameof, ":\n ", p)
show(io::IO, ::MIME"text/plain", truth_table::TruthTable) =
    print_truth_table(io, truth_table)
