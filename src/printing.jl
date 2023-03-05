
import Base: show, print
import AbstractTrees: children, nodevalue, print_tree

using PrettyTables

"""
    Pretty
"""
struct Pretty{P <: Proposition}
    p::P
    text::String

    Pretty(p::P, text::String = replace(_show(p), "\"" => "")) where P <: Proposition = new{P}(p, text)
end

"""
    @pretty(expression)
"""
macro pretty(expression)
    return :(Pretty($(esc(expression)), $(string(expression))))
end


children(p::Tree) = Tuple(p.p)
children(p::Tree{typeof(not)}) = p.p
children(p::Tree{typeof(identity)}) = ()

nodevalue(p::Tree{BO}) where BO <: BooleanOperator = BO.instance
nodevalue(p::Tree{typeof(identity)}) = p

"""
    print_tree(p::Proposition, [max_depth])

Print a tree diagram of ```p```.

The optional argument `max_depth` will truncate subtrees at that depth.

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
print_tree(p::Tree, max_depth = typemax(Int)) = print_tree(p, maxdepth = max_depth)
print_tree(p::Proposition, max_depth = typemax(Int)) = print_tree(Tree(p), max_depth)

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
    truth_table(xs...)
    truth_table(xs::AbstractArray)

Print a truth table for the given [`Proposition`](@ref)s and [`BinaryOperator`](@ref)s.

The first row of the header is the expression representing that column's proposition,
while the second row indicates that expression's type.
Logically equivalent propositions will be grouped in the same column, seperated by a comma.

In this context, [`⊤`](@ref tautology) and [`⊥`](@ref contradiction) can be interpreted as *true* and *false*, respectively.

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

julia> truth_table([⊻, imply])
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
function truth_table(xs::AbstractArray)
    # ToDo: write docstring - define behavior
    # ToDo: write tests
    # TODO: fix `truth_table(Valuation(⊤))`
    # TODO: make header support operators (`⊤; NullaryOperator`, `⊻; BinaryOperator`)

    operator_to_proposition = x -> begin
        x isa NullaryOperator && return Clause(x)
        x isa UnaryOperator && return x(Atom(:_))
        x isa BinaryOperator && return x(Atom(:_), Atom(:__))
        return x
    end

    ps = map(operator_to_proposition, xs)
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
    interpretations = mapreduce(hcat, grouped_ps) do grouped_p
        get_interpretation(first(grouped_p), valuations)
    end

    merge_string = x -> join(x, ", ")
    header = (
        map(grouped_ps) do group
            merge_string(map(repr, group))
        end,
        map(grouped_ps) do group
            merge_string(map(nameof ∘ typeof, group))
        end
    )

    pretty_table(
        interpretations,
        header = header,
        alignment = :l,
        body_hlines = collect(0:2:2^length(atoms)),
        crop = :none
    )

    return nothing
end
truth_table(xs...) = truth_table(collect(xs))

"""
    @truth_table(xs...)

Equivalent to `[`@p`](@ref) [`truth_table`](@ref)(xs...)`.

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

julia> @truth_table (⊻) imply
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
macro truth_table(xs...)
    return esc(:(truth_table($(map(atomize, xs)...))))
end

struct Proof
    xs::Vector{Pair}

    # Proof(p::P) where P <: Proof
    Proof(p::P) where P <: Proposition = interpret(new([p => "Assumption"]))
end

Proof(premises, conclusion) = Proof(imply(reduce(and, premises), conclusion))

show(io::IO, ::MIME"text/plain", proof::Proof) = pretty_table(
    io,
    reduce(
        hcat,
        [1:length(proof.xs), map(first, proof.xs), map(last, proof.xs)]
    ),
    header = ["#", "Formula", "Reason"],
    alignment = :l,
    crop = :none
)

parenthesize(p::Union{Literal, Tree{<:UnaryOperator}}) = _show(p)
parenthesize(p::Union{Clause, Tree{<:BinaryOperator}}) = "(" * _show(p) * ")"

_show(::typeof(tautology)) = "⊤"
_show(::typeof(contradiction)) = "⊥"
_show(::typeof(identity)) = ""
_show(::typeof(not)) = "¬"
_show(::typeof(left)) = "≺"
_show(::typeof(not_left)) = "⊀"
_show(::typeof(right)) = "≻"
_show(::typeof(not_right)) = "⊁"
_show(::typeof(and)) = "∧"
_show(::typeof(nand)) = "⊼"
_show(::typeof(nor)) = "⊽"
_show(::typeof(or)) = "∨"
_show(::typeof(xor)) = "⊻"
_show(::typeof(xnor)) = "↔"
_show(::typeof(imply)) = "→"
_show(::typeof(not_imply)) = "↛"
_show(::typeof(converse_imply)) = "←"
_show(::typeof(not_converse_imply)) = "↚"
_show(p::Atom{Symbol}) = string(p.p)
_show(p::Atom{String}) = "\"" * p.p * "\""
function _show(p::Valuation) # TODO: improve, support `[Valuation(and, p), etc]`
    s = ""

    for interpretation in p.p
        s *= "["
        s *= join(map(x -> _show(first(x)) * " => " * _show(last(x)), first(interpretation)), ", ")
        s *= "] => " * _show(last(interpretation))
        if interpretation != last(p.p)
            s *= "\n "
        end
    end
    return s

    x = repr("text/plain", p.p)
    i = last(findfirst("\n ", x))
    return x[i + 1:end]
end
_show(p::Tuple{Proposition}) = _show(only(p))
_show(p::Union{Literal{UO}, Tree{UO}}) where UO <: UnaryOperator = _show(UO.instance) * _show(p.p)
_show(p::Tree{BO}) where BO <: BinaryOperator = parenthesize(first(p.p)) * " " * _show(BO.instance) * " " * parenthesize(last(p.p))
function _show(p::Union{Clause{AO}, Normal{AO}}) where AO <: AndOr
    isempty(p.p) && return _show(identity(left, AO.instance))
    return join(map(parenthesize, p.p), " " * _show(AO.instance) * " ")
end

show(io::IO, p::Union{BooleanOperator, Proposition}) = print(io, _show(p))
show(io::IO, ::MIME"text/plain", p::P) where P <: Proposition = print(io, nameof(P), ":\n ", _show(p))

print(io::IO, ::BO) where BO <: BooleanOperator = show(io, BO.instance)
