
using Combinatorics
using PrettyTables

import Base.length, Base.print, Base.show

"""
    @primitive(ps...)

Instantiates [`Primitive`](@ref) propositions.

Examples
```jldoctest
julia> @primitive p q

julia> p
Primitive("p")

julia> q
Primitive("q")
```
"""
macro primitive(expressions...)
    primitive = expression -> :($(esc(expression)) = Primitive($(string(expression))))
    primitives = map(primitive, expressions)

    return quote
        $(primitives...)
        nothing
    end
end
#=
Source:
https://github.com/ctrekker/Deductive.jl
=#

"""
    get_primitives(ps...)

Returns a vector of [`Primitive`](@ref) propositions contained in ```p```.

Note that some primitives may optimized out of a statement, such as in ```p ∧ ⊥```.

# Examples
```jldoctest
julia> get_primitives(p)
1-element Vector{Primitive}:
 Primitive("p")

julia> get_primitives(p ∧ q, r)
3-element Vector{Primitive}:
 Primitive("p")
 Primitive("q")
 Primitive("r")
```
"""
get_primitives(ps::Language...) = union(mapreduce(get_primitives, vcat, ps))
get_primitives(p::Compound) = union(get_primitives(p.ϕ))
get_primitives(ϕ::Tuple{Operator, Vararg}) = mapreduce(p -> get_primitives(p.ϕ), vcat, Base.tail(ϕ))
get_primitives(p::Primitive) = [p]
get_primitives(::Truth) = Primitive[]

(p::Truth)(worlds) = p
(p::Primitive)(worlds) = worlds[p]
(p::Propositional{<:Primitive})(worlds) = p.ϕ(worlds)
(p::Propositional{<:Tuple})(worlds) = first(p.ϕ)(map(ϕ -> ϕ(worlds), Base.tail(p.ϕ))...)
"""
    interpret(valuation, ϕ::Union{Primitive, Compound})

Given a valuation, 

A valuation is a function that maps from the
[`Primitive`](@ref) propositions in ```ϕ``` to their respective [`Truth`](@ref) values.

See also [`Primitive`](@ref), [`Compound`](@ref), and [`Truth`](@ref).

```jldoctest
julia> mapping = Dict(p => ⊥, q => ⊤);

julia> valuation = r -> mapping[r];

julia> interpret(valuation, p ∧ q)
⊥

julia> interpret(valuation, p → q)
⊤
```
"""
interpret(valuation, ϕ::Language) = ϕ(Dict(map(p -> p => valuation(p), get_primitives(ϕ))))

# TODO: simplify logic
# TODO: fix subheader of `@truth_table ⊥ p ∧ ¬p`
# TODO: fix `@truth_table p ∧ ¬(p ∧ ¬p)`
function truth_table(trees, trees_str, leaves, leaves_str)
    primitives = get_primitives(trees...)
    n = length(primitives)
    truth_sets = multiset_permutations([⊤, ⊥], [n, n], n)
    valuations = map(truth_set -> zip(primitives, truth_set), truth_sets)

    merge_string = (x, y) -> x == y || y == "" ? x : x * ", " * y

    _sub_header = Language[]
    labels = String[]
    assignments = Vector{Truth}[]
    for (tree, tree_str) in zip(trees, trees_str)
        truths = Truth[]

        if tree isa Primitive
            continue
        end

        for valuation in valuations
            push!(truths, interpret(p -> Dict(valuation)[p], tree))
        end

        if truths in assignments
            i = findfirst(assignment -> assignment == truths, assignments)
            labels[i] = merge_string(labels[i], tree_str)
        else
            push!(_sub_header, tree)
            push!(labels, tree_str)
            push!(assignments, truths)
        end
    end

    valuation_matrix = mapreduce(permutedims, vcat, truth_sets)
    assignment_matrix = reduce(hcat, assignments, init = Matrix(undef, 2^n, 0))
    interpretations = hcat(valuation_matrix, assignment_matrix)

    make_header = (ps, ps_str) -> begin
        ___header = Dict{Primitive, Vector{String}}()

        for (p, p_str) in zip(ps, ps_str)
            if p isa Primitive
                if p in keys(___header)
                    push!(___header[p], p_str)
                else
                    ___header[p] = [p_str]
                end
            end
        end

        return ___header
    end

    header_domains = [
        (leaves, leaves_str),
        (trees, trees_str),
        (primitives, map(primitive -> "", primitives))
    ]
    headers = map(header_domain -> make_header(header_domain...), header_domains)
    __header = mergewith!(union ∘ vcat, headers...)
    _header = map(primitive -> reduce(merge_string, __header[primitive]), primitives)
    push!(_header, labels...)

    sub_header = [primitives; map(nameof ∘ typeof, _sub_header)]
    header = (_header, sub_header)

    pretty_table(
        interpretations,
        header = header,
        body_hlines = collect(0:2:2^n),
        crop = :none
    )
end
"""
    @truth_table(ps...)

Print a truth table for the given [`Compound`](@ref) propositions.

See also [`Primitive`](@ref) and [`Compound`](@ref).

# Examples
```jldoctest
julia> @truth_table p∧q p→q
┌────────────────┬────────────────┬───────────────┬───────────────┐
│              p │              q │         p ∧ q │         p → q │
│ Primitive("p") │ Primitive("q") │ Propositional │ Propositional │
├────────────────┼────────────────┼───────────────┼───────────────┤
│              ⊤ │              ⊤ │             ⊤ │             ⊤ │
│              ⊤ │              ⊥ │             ⊥ │             ⊥ │
├────────────────┼────────────────┼───────────────┼───────────────┤
│              ⊥ │              ⊤ │             ⊥ │             ⊤ │
│              ⊥ │              ⊥ │             ⊥ │             ⊤ │
└────────────────┴────────────────┴───────────────┴───────────────┘
```
"""
macro truth_table(expressions...)
    f = expression -> typeof(expression) <: Union{Symbol, String} ? [expression] : mapreduce(f, vcat, expression.args[2:end])
    propositions = reduce(union, map(f, expressions))

    return :(
        truth_table(
            [$(map(esc, expressions)...)],
            map(string, $expressions),
            [$(map(esc, propositions)...)],
            map(string, $propositions)
        )
    )
end

"""
    equivalent(p::Language, q::Language)

Returns a boolean of whether propositions p and q are logically equivalent.

# Examples
```

```
"""
equivalent(p::Language, q::Language) = p ↔ q == ⊤
# import Base.==
# Base.==(p::Language, q::Language) = equivalent(p, q)










depth(ϕ::Compound) = depth(ϕ.ϕ)
# depth(ϕ::Tuple{temp_modal, Agent, Language}) = 1 + depth(Base.tail(ϕ))
depth(ϕ::Tuple{Boolean, Vararg}) = maximum(depth, Base.tail(ϕ))
depth(p::Primitive) = 0

# length(ϕ::Compound) = length(ϕ.ϕ)
# length(p::Primitive) = 1
# length(ϕ::Tuple{Boolean, Vararg}) = 1 + mapreduce(length, +, Base.tail(ϕ))
# length(ϕ::Tuple{Modal, Agent, Language}) = 1 + length(Base.tail(ϕ))


# #=
# returns
#     ⊤ if tautology
#     ⊥ if contradiction
#     ? if contingency
# =#
# # satisfiable
# # unsatisfiable

# # return vector of possible worlds
# # if no possible worlds, contradiction
# # if ϕ is in all possible worlds, tautology
# # if ϕ is in no possible worlds, contradiction
# prove(w::world, f::Language) = w.states[f](f)

# ⊢ # \vdash
# ⊣ # \dashv

# ≡(ϕ::Language, ψ) = ϕ() == ψ() # \equiv - logical equivalence
# equivalent = ≡
