
import Base: show
import AbstractTrees # print_tree
import AbstractTrees: children, nodevalue, printnode

using PrettyTables: LatexCell, pretty_table
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
    TruthTable(ps)

Construct a [truth table](https://en.wikipedia.org/wiki/Truth_table)
for the given [`Proposition`](@ref)s and [`LogicalOperator`](@ref)s.

The `header` is a vector containing vectors of logically equivalent propositions.
The `sub_header` corresponds to the `header`, but contains each proposition's `UnionAll` type.
The `body` is a matrix where the rows contain [`interpretations`](@ref)
and the columns correspond to elements in the `header` and `sub_header`.

Logically equivalent propositions are grouped in the same column.

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
    body::Matrix{NullaryOperator}

    function TruthTable(ps)
        # ToDo: write docstring - define behavior
        # ToDo: write tests
        # TODO: make header support operators (`⊤; NullaryOperator`, `⊻; BinaryOperator`)

        ps = Iterators.map(operator_to_proposition, ps)
        _atoms = mapreduce(atoms, union, ps)
        ps = union(_atoms, ps)
        _valuations = valuations(_atoms)
        _interpretations = Iterators.map(p -> collect(interpretations(p, _valuations)), ps)

        truths_interpretations, atoms_interpretations, compounds_interpretations =
            Vector{NullaryOperator}[], Vector{NullaryOperator}[], Vector{NullaryOperator}[]

        group = xs -> Dict(map(
            x -> collect(interpretations(x, _valuations)) => Proposition[],
            xs
        ))
        grouped_truths, grouped_atoms = map(group, ((tautology, contradiction), _atoms))
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
        sub_header = Vector{UnionAll}[]
        body = Vector{NullaryOperator}[]
        for (_interpretations, group) in (
            truths_interpretations => grouped_truths,
            atoms_interpretations => grouped_atoms,
            compounds_interpretations => grouped_compounds
        )
            for interpretation in _interpretations
                xs = get(group, interpretation, Proposition[])
                push!(header, xs)
                push!(sub_header, map(x -> getfield(Main, nameof(typeof(x))), xs))
                push!(body, interpretation)
            end
        end

        new(header, sub_header, reduce(hcat, body))
    end
end
TruthTable(p::Union{LogicalOperator, Proposition}) = TruthTable((p,))
TruthTable(ps...) = TruthTable(ps)

operator_to_symbol(::typeof(identity)) = ""
for operator_symbol in (:⊤, :⊥, :¬, :∧, :⊼, :⊽, :∨, :⊻, :↔, :→, :↛, :←, :↚)
    @eval operator_to_symbol(::typeof($operator_symbol)) = $(string(operator_symbol))
end

letter(::typeof(tautology)) = "T"
letter(::typeof(contradiction)) = "F"

format_latex(x) = LatexCell(print_latex(String, x))

format_head(format, cell) = (format == :latex ? format_latex : identity)(cell)

const _format_body = Dict(
    :truth => string ∘ operator_to_symbol,
    :text => string ∘ nameof,
    :letter => letter,
    :bool => string ∘ Bool,
    :bit =>  string ∘ Int ∘ Bool,
    :latex => format_latex ∘ operator_to_symbol
)

format_body(format, cell) = _format_body[format](cell)

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

merge_string(cell) = join(Iterators.map(
    p -> sprint(
        (io, q) -> show(io, MIME"text/plain"(), q), p
    ), cell
), ", ")

function __print_truth_table(
    backend, io, truth_table;
    sub_header = true, numbered_rows = false, format = :truth, alignment = :l,
    kwargs...
)
    header = map(cell -> merge_string(format_head(format, cell)), truth_table.header)
    if sub_header
        header = (header, map(merge_string, truth_table.sub_header))
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

_newline(newline) = newline ? "\n" : ""

children(p::Tree) = p.nodes
children(p::Tree{typeof(identity)}) = ()

nodevalue(p::Tree{LO}) where LO = LO.instance
nodevalue(p::Tree{typeof(identity)}) = p

printnode(io::IO, node::Union{Atom, Tree{typeof(identity)}}) = show(io, MIME"text/plain"(), nodevalue(node))
printnode(io::IO, node::Tree{LO}) where LO = print(io, operator_to_symbol(LO.instance))

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
print_tree(io::IO, p::Tree; max_depth = typemax(Int), newline = false, kwargs...) =
    print(io,
        rstrip(print_string(AbstractTrees.print_tree, p; maxdepth = max_depth, kwargs...)),
        _newline(newline)
    )
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

function print_string(f, args...; kwargs...)
    buffer = IOBuffer()
    f(buffer, args...; kwargs...)
    String(take!(buffer))
end

for f in (:tree, :latex, :truth_table, :markdown)
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

only_field(p::Atom) = p.statement
only_field(p::Literal) = p.atom
only_field(p::Tree) = p.nodes
only_field(p::Clause) = p.literals
only_field(p::Normal) = p.clauses

union_all_type(p::Atom) = Atom
union_all_type(p::Literal) = Literal
union_all_type(p::Tree) = Tree
union_all_type(p::Clause) = Clause
union_all_type(p::Normal) = Normal

parenthesize(io, p) = show(io, MIME"text/plain"(), p)
function parenthesize(io, p::Union{Clause, Tree{<:BinaryOperator}})
    print(io, "(")
    show(io, MIME"text/plain"(), p)
    print(io, ")")
end

"""
    show
"""
function show(io::IO, p::A) where A <: Atom
    print(io, nameof(A), "(")
    show(io, p.statement)
    print(io, ")")
end
show(io::IO, p::L) where {UO, L <: Literal{UO}} =
    print(io, nameof(L), "(", UO.instance, ", ", p.atom, ")")
show(io::IO, p::T) where {LO, T <: Tree{LO}} = print(io,
    nameof(T), "(", LO.instance, ", ", sprint((io, node) -> join(io, node, ", "), p.nodes), ")"
)
show(io::IO, p::CN) where {AO, CN <: Union{Clause{AO}, Normal{AO}}} = print(io,
    nameof(CN), "(", AO.instance, ", [", sprint((io, xs) -> join(io, xs, ", "), only_field(p)), "])"
)
show(io::IO, ::MIME"text/plain", p::Atom) = show(io, p)
show(io::IO, ::MIME"text/plain", p::Atom{Symbol}) = print(io, p.statement)
function show(io::IO, ::MIME"text/plain", p::Literal{UO}) where UO
    print(io, operator_to_symbol(UO.instance))
    show(io, MIME"text/plain"(), p.atom)
end
show(io::IO, ::MIME"text/plain", p::Tree{NO, <:Tuple{}}) where NO =
    print(io, operator_to_symbol(NO.instance))
function show(io::IO, ::MIME"text/plain", p::Tree{UO, <:Tuple{Atom}}) where UO
    print(io, operator_to_symbol(UO.instance))
    show(io, MIME"text/plain"(), only(p.nodes))
end
function show(io::IO, ::MIME"text/plain", p::Tree{UO, <:Tuple{Tree}}) where UO
    print(io, operator_to_symbol(UO.instance), "(")
    show(io, MIME"text/plain"(), only(p.nodes))
    print(io, ")")
end
function show(io::IO, ::MIME"text/plain", p::Tree{BO, <:NTuple{2, Tree}}) where BO
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
