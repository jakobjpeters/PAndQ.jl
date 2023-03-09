
import Base: show, print
import AbstractTrees: children, nodevalue, print_tree

using PrettyTables

children(p::Tree) = Tuple(p.node)
children(p::Tree{typeof(not)}) = p.node
children(p::Tree{typeof(identity)}) = ()

nodevalue(p::Tree{BO}) where BO <: BooleanOperator = BO.instance
nodevalue(p::Tree{typeof(identity)}) = p

"""
    print_tree(p, max_depth = typemax(Int64))

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
print_tree(p::Tree, max_depth = typemax(Int64)) = print_tree(p, maxdepth = max_depth)
print_tree(p::Proposition, max_depth = typemax(Int64)) = print_tree(Tree(p), max_depth)

# print_string(p::Proposition)

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
    truth_table(::AbstractArray; numbered_rows = false)
    truth_table(ps...; numbered_rows = false)

Print a [truth table](https://en.wikipedia.org/wiki/Truth_table)
for the given [`Proposition`](@ref)s and [`BinaryOperator`](@ref)s.

If `numbered_rows = true`, the first column will contain each row's sequential number.

The first row of the header is the expression representing that column's proposition,
while the second row indicates that expression's type.
Logically equivalent propositions will be grouped in the same column, seperated by a comma.

See also [`tautology`](@ref) and [`contradiction`](@ref).

# Examples
```jldoctest
julia> @p truth_table(p ∧ ¬p, p ∧ q)
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

julia> truth_table([⊻, imply], numbered_rows = true)
┌───┬──────┬──────┬────────┬────────┐
│ # │ _    │ __   │ _ ⊻ __ │ _ → __ │
│   │ Atom │ Atom │ Tree   │ Tree   │
├───┼──────┼──────┼────────┼────────┤
│ 1 │ ⊤    │ ⊤    │ ⊥      │ ⊤      │
│ 2 │ ⊥    │ ⊤    │ ⊤      │ ⊤      │
├───┼──────┼──────┼────────┼────────┤
│ 3 │ ⊤    │ ⊥    │ ⊤      │ ⊥      │
│ 4 │ ⊥    │ ⊥    │ ⊥      │ ⊤      │
└───┴──────┴──────┴────────┴────────┘
```
"""
function truth_table(ps::AbstractArray; numbered_rows = false)
    # ToDo: write docstring - define behavior
    # ToDo: write tests
    # TODO: fix `truth_table(Valuation(⊤))`?
    # TODO: make header support operators (`⊤; NullaryOperator`, `⊻; BinaryOperator`)

    operator_to_proposition = p -> begin
        p isa NullaryOperator && return Clause(p)
        p isa UnaryOperator && return p(Atom(:_))
        p isa BinaryOperator && return p(Atom(:_), Atom(:__))
        return p
    end

    ps = map(operator_to_proposition, ps)
    # atoms = get_atoms(map(interpret ∘ Valuation, ps)) # TODO: only atoms that affect the outcome (p ∧ ¬p ∨ q)
    atoms = mapreduce(get_atoms, union, ps)
    grouped_ps = Vector{Proposition}[]

    foreach(ps) do p
        equivalent = false
        for group in grouped_ps
            if p == first(group)
                push!(group, p)
                equivalent = true
                break
            end
        end
        !equivalent && push!(grouped_ps, [p])
    end

    grouped_truths = Vector{Proposition}[]
    grouped_atoms = Vector{Proposition}[]
    grouped_compounds = Vector{Proposition}[]

    append!(grouped_atoms, map(p -> [p], atoms))
    foreach(grouped_ps) do group
        if is_truth(first(group))
            push!(grouped_truths, group)
        else
            is_atom = false
            for grouped_atom in grouped_atoms
                if first(grouped_atom) == first(group)
                    append!(grouped_atom, group)
                    is_atom = true
                    break
                end
            end

            !is_atom && push!(grouped_compounds, group)
        end
    end

    grouped_ps = map(unique, vcat(grouped_truths, grouped_atoms, grouped_compounds))

    valuations = map(Dict, get_valuations(atoms))
    body = mapreduce(hcat, grouped_ps) do grouped_p
        get_interpretation(first(grouped_p), valuations)
    end

    merge_string = x -> join(x, ", ")

    header = map(grouped_ps) do group
        merge_string(map(repr, group))
    end
    sub_header = map(grouped_ps) do group
        merge_string(map(nameof ∘ typeof, group))
    end

    n_rows = 2^length(atoms)

    if numbered_rows
        pushfirst!(header, "#")
        pushfirst!(sub_header, "")
        body = hcat(1:n_rows, body)
    end

    return pretty_table(
        body,
        header = (header, sub_header),
        body_hlines = collect(0:2:n_rows),
        alignment = :l,
        crop = :none
    )
end
truth_table(ps...; numbered_rows = false) = truth_table(collect(ps), numbered_rows = numbered_rows)

"""
    @truth_table(ps...; numbered_rows = false)

Equivalent to `@p truth_table(ps...; numbered_rows = false)`.

See also [`@p`](@ref) and [`truth_table`](@ref).

# Examples
```jldoctest
julia> @truth_table ¬p Clause(and, p, q)
┌──────┬──────┬─────────┬────────┐
│ p    │ q    │ ¬p      │ p ∧ q  │
│ Atom │ Atom │ Literal │ Clause │
├──────┼──────┼─────────┼────────┤
│ ⊤    │ ⊤    │ ⊥       │ ⊤      │
│ ⊥    │ ⊤    │ ⊤       │ ⊥      │
├──────┼──────┼─────────┼────────┤
│ ⊤    │ ⊥    │ ⊥       │ ⊥      │
│ ⊥    │ ⊥    │ ⊤       │ ⊥      │
└──────┴──────┴─────────┴────────┘

julia> @truth_table (⊻) imply numbered_rows = true
┌───┬──────┬──────┬────────┬────────┐
│ # │ _    │ __   │ _ ⊻ __ │ _ → __ │
│   │ Atom │ Atom │ Tree   │ Tree   │
├───┼──────┼──────┼────────┼────────┤
│ 1 │ ⊤    │ ⊤    │ ⊥      │ ⊤      │
│ 2 │ ⊥    │ ⊤    │ ⊤      │ ⊤      │
├───┼──────┼──────┼────────┼────────┤
│ 3 │ ⊤    │ ⊥    │ ⊤      │ ⊥      │
│ 4 │ ⊥    │ ⊥    │ ⊥      │ ⊤      │
└───┴──────┴──────┴────────┴────────┘
```
"""
macro truth_table(ps...)
    numbered_rows = Meta.isexpr(last(ps), :(=))
    return esc(:(truth_table(
        $(map(atomize, ps[1:length(ps) - numbered_rows])...);
        $(numbered_rows ? last(ps) : :(numbered_rows = false))
    )))
end

parenthesize(p::Union{Literal, Tree{<:UnaryOperator}}) = _show(p)
parenthesize(p::Union{Clause, Tree{<:BinaryOperator}}) = reduce(*, ["(", _show(p), ")"])

_show(::typeof(identity)) = ""
foreach([:⊤, :⊥, :¬, :∧, :⊼, :⊽, :∨, :⊻, :↔, :→, :↛, :←, :↚]) do boolean_operator
    @eval _show(::typeof($boolean_operator)) = $(string(boolean_operator))
end
_show(p::Atom{Symbol}) = string(p.statement)
_show(p::Atom{String}) = "\"" * p.statement * "\""
function _show(p::Valuation) # TODO: improve, support `[Valuation(and, p), etc]`
    return join(
        map(p.interpretations) do interpretation
            reduce(*, [
                "[",
                join(
                    map(first(interpretation)) do pair
                        _show(first(pair)) * " => " * _show(last(pair))
                    end,
                    ", "
                ),
                "] => ",
                _show(last(interpretation))
            ])
        end,
        "\n "
    )
end
_show(p::Tuple{Proposition}) = _show(only(p))
_show(p::Union{Literal{UO}, Tree{UO}}) where UO <: UnaryOperator =
    _show(UO.instance) * _show(getfield(p, 1))
_show(p::Tree{BO}) where BO <: BinaryOperator = join([
    parenthesize(first(p.node)),
    _show(BO.instance),
    parenthesize(last(p.node))
], " ")
function _show(p::Union{Clause{AO}, Normal{AO}}) where AO <: AndOr
    isempty(getfield(p, 1)) && return _show(identity(:left, AO.instance))
    return join(map(parenthesize, getfield(p, 1)), join(repeat([" "], 2), _show(AO.instance)))
end

"""
    show
"""
show(io::IO, p::Union{BooleanOperator, Proposition}) = print(io, _show(p))
show(io::IO, ::MIME"text/plain", p::P) where P <: Proposition =
    print(io, nameof(P), ":\n ", _show(p))

"""
    print
"""
print(io::IO, ::BO) where BO <: BooleanOperator = show(io, BO.instance)
