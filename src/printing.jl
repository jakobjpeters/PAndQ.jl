
import Base: show, print
import AbstractTrees # print_tree
import AbstractTrees: children, nodevalue

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

operator_to_proposition(x::NullaryOperator) = Clause(x)
operator_to_proposition(x::UnaryOperator) = x(Atom(:_))
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

    function TruthTable(ps::AbstractArray)
        # ToDo: write docstring - define behavior
        # ToDo: write tests
        # TODO: make header support operators (`⊤; NullaryOperator`, `⊻; BinaryOperator`)

        ps = map(operator_to_proposition, ps)
        _atoms = mapreduce(atoms, union, ps)
        ps = union(_atoms, ps)
        _valuations = valuations(_atoms)
        _interpretations = map(p -> interpretations(p, _valuations), ps)

        truths_interpretations, atoms_interpretations, compounds_interpretations =
            Vector{Function}[], Vector{Function}[], Vector{Function}[]

        group = xs -> Dict(map(xs) do x
            interpretations(x, _valuations) => Proposition[]
        end)
        grouped_truths, grouped_atoms = map(group, ([tautology, contradiction], _atoms))
        grouped_compounds = Dict{Vector{Function}, Vector{Proposition}}()

        foreach(zip(ps, _interpretations)) do (p, interpretation)
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
        sub_header = Vector{UnionAll}[]
        body = Vector{Function}[]
        foreach([
            truths_interpretations => grouped_truths,
            atoms_interpretations => grouped_atoms,
            compounds_interpretations => grouped_compounds
        ]) do (interpretations, group)
            foreach(interpretations) do interpretation
                xs = get(group, interpretation, Proposition[])
                push!(header, xs)
                push!(sub_header, map(x -> getfield(Main, nameof(typeof(x))), xs))
                push!(body, interpretation)
            end
        end
        return new(header, sub_header, reduce(hcat, body))
    end
end
TruthTable(ps...) = TruthTable(collect(ps))

letter(::typeof(tautology)) = "T"
letter(::typeof(contradiction)) = "F"

format_latex(x) = LatexCell(print_latex(String, x))

format_head(::Val{:latex}, x) = x |> format_latex
format_head(::Val, x) = x

format_body(::Val{:truth}, x) = x |> string
format_body(::Val{:text}, x) = x |> nameof |> string
format_body(::Val{:letter}, x) = x |> letter
format_body(::Val{:bool}, x) = x |> Bool |> string
format_body(::Val{:bit}, x) = x |> Bool |> Int |> string
format_body(::Val{:latex}, x) = x |> format_latex

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

merge_string(x) = join(x, ", ")

function __print_truth_table(
    backend, io, truth_table;
    sub_header = true, numbered_rows = false, format = :truth, alignment = :l,
    kwargs...
)
    header = map(truth_table.header) do p
        merge_string(format_head(Val(format), p))
    end
    sub_header && (header = (header, map(merge_string, truth_table.sub_header)))

    body = map(truth_table.body) do cell
        format_body(Val(format), cell)
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
    _print_truth_table(Val(backend), io, truth_table; kwargs...)
print_truth_table(io::IO, x; kwargs...) =
    print_truth_table(io, TruthTable(x); kwargs...)
print_truth_table(x; kwargs...) =
    print_truth_table(stdout, x; kwargs...)

children(p::Tree) = Tuple(p.node)
children(p::Tree{typeof(not)}) = p.node
children(p::Tree{typeof(identity)}) = ()

nodevalue(p::Tree{BO}) where BO <: BooleanOperator = BO.instance
nodevalue(p::Tree{typeof(identity)}) = p

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
function print_tree(io::IO, p::Tree; max_depth = typemax(Int), newline = false, kwargs...)
    print(io, rstrip(print_string(AbstractTrees.print_tree, p; maxdepth = max_depth, kwargs...)))
    newline && println(io)
    return nothing
end
print_tree(io::IO, p; kwargs...) = print_tree(io, Tree(p); kwargs...)
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
function print_latex(io::IO, x::String; newline = false, delimeter = "\\(" => "\\)")
    # populates `symbols_latex`
    isempty(symbols_latex) && symbol_latex("∧")

    latex = join([
        first(delimeter),
        rstrip(mapreduce(*, x) do c
            c == ' ' ? "" : get(symbols_latex, string(c), c) * " "
        end),
        last(delimeter)
    ])

    print(io, latex)
    newline && println(io)
    return nothing
end
print_latex(io::IO, x::TruthTable; kwargs...) =
    print_truth_table(io, x; backend = :latex, kwargs...)
print_latex(io::IO, x; kwargs...) = print_latex(io, string(x); kwargs...)
print_latex(x; kwargs...) = print_latex(stdout, x; kwargs...)

"""
    print_markdown

# Examples
"""
print_markdown(::Type{MD}, p) = MD(string(p))
print_markdown(::Type{MD}, truth_table::TruthTable; alignment = :c) =
    MD(Table(
        [first(truth_table.header), eachrow(map(string, truth_table.body))...],
        repeat([alignment], length(first(truth_table.header)))
    ))
function print_markdown(io::IO, x; newline = false, kwargs...)
    print(io, string(print_markdown(MD, x; kwargs...))[begin:end - 1])
    newline && println(io)
    return nothing
end
print_markdown(x; kwargs...) = print_markdown(stdout, x; kwargs...)

function print_string(f, args...; kwargs...)
    buffer = IOBuffer()
    f(buffer, args...; kwargs...)
    return buffer |> take! |> String
end

foreach([:tree, :latex, :truth_table, :markdown]) do f
    print_f = Symbol("print_" * string(f))
    println_f = Symbol("println_" * string(f))
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

parenthesize(p::Union{Literal, Tree{<:UnaryOperator}}) = p |> _show
parenthesize(p::Union{Clause, Tree{<:BinaryOperator}}) = "(" * _show(p) * ")"

_show(::typeof(identity)) = ""
foreach([:⊤, :⊥, :¬, :∧, :⊼, :⊽, :∨, :⊻, :↔, :→, :↛, :←, :↚, :⋀, :⋁]) do boolean_operator
    @eval _show(::typeof($boolean_operator)) = $(string(boolean_operator))
end
_show(p::Atom{Symbol}) = string(p.statement)
_show(p::Atom{String}) = "\"" * p.statement * "\""
_show(p::Tuple{Proposition}) = _show(only(p))
_show(p::Union{Literal{UO}, Tree{UO}}) where UO <: UnaryOperator =
    _show(UO.instance) * _show(getfield(p, 1))
_show(p::Tree{BO}) where BO <: BinaryOperator = join([
    parenthesize(first(p.node)),
    _show(BO.instance),
    parenthesize(last(p.node))
], " ")
_show(p::Union{Clause{AO}, Normal{AO}}) where AO <: AndOr =
    isempty(getfield(p, 1)) ?
        _show(identity(:left, AO.instance)) :
        join(map(parenthesize, getfield(p, 1)), join(repeat([" "], 2), _show(AO.instance)))

"""
    show
"""
show(io::IO, p::Union{BooleanOperator, Proposition}) = print(io, _show(p))
show(io::IO, ::MIME"text/plain", p::P) where P <: Proposition =
    print(io, nameof(P), ":\n ", _show(p))
show(io::IO, ::MIME"text/plain", truth_table::TruthTable) =
    print_truth_table(io, truth_table)

"""
    print
"""
print(io::IO, ::BO) where BO <: BooleanOperator = show(io, BO.instance)
