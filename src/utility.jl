
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
    primitives(ps...)

Returns a vector of [`Primitive`](@ref) propositions contained in ```p```.

Note that some primitives may optimized out of a statement, such as in ```p ∧ ⊥```.

# Examples
```jldoctest; setup = :(@primitive p q r)
julia> primitives(p)
1-element Vector{Primitive}:
 Primitive("p")

julia> primitives(p ∧ q, r)
3-element Vector{Primitive}:
 Primitive("p")
 Primitive("q")
 Primitive("r")
```
"""
primitives(ps::Language...) = union(mapreduce(primitives, vcat, ps))
primitives(p::Compound) = union(primitives(p.ϕ))
primitives(ϕ::Tuple{Operator, Vararg}) = mapreduce(p -> primitives(p.ϕ), vcat, Base.tail(ϕ))
primitives(p::Primitive) = [p]

(p::Primitive)(worlds) = worlds[p]
(p::Propositional{<:Primitive})(states) = p.ϕ(states)
(p::Propositional{<:Tuple})(worlds) = first(p.ϕ)(map(ϕ -> ϕ(worlds), Base.tail(p.ϕ))...)
"""
    interpret(valuation, ϕ::Union{Primitive, Compound})

Given a valuation, 

A valuation is a function that maps from the
[`Primitive`](@ref) propositions in ```ϕ``` to their respective [`Truth`](@ref) values.

See also [`Primitive`](@ref), [`Compound`](@ref), and [`Truth`](@ref).

```jldoctest; setup = :(@primitive p q)
julia> mapping = Dict(p => ⊥, q => ⊤);

julia> valuation = r -> mapping[r];

julia> interpret(valuation, p ∧ q)
⊥

julia> interpret(valuation, p → q)
⊤
```
"""
interpret(valuation, ϕ::Language) = ϕ(Dict(map(p -> p => valuation(p), primitives(ϕ))))

# TODO: use ordered dicts?
function truth_table(trees, nodes)
    cs = first.(filter(tree -> first(tree) isa Compound, trees))
    ps = primitives(cs...)

    comma = (x, y) -> x == y ? x : x * ", " * y
    p_map = mergewith!(
        comma,
        # TODO: simplify?
        Dict(map(p -> p => p.statement, filter(p -> !in(p, map(first, nodes)), ps))),
        Dict.(filter(node -> first(node) isa Primitive, nodes))...
    )

    n = length(ps)
    truth_sets = multiset_permutations([⊤, ⊥], [n, n], n)
    valuations = map(truth_set -> zip(ps, truth_set), truth_sets)

    c_map = Dict()
    for c in trees
        x = Truth[]
        for valuation in valuations
            push!(x, interpret(p -> Dict(valuation)[p], first(c)))
        end
        mergewith!(comma, c_map, Dict(x => last(c)))
    end

    interpretations = mapreduce(permutedims, vcat, truth_sets)
    interpretations = reduce(hcat, keys(c_map), init = interpretations)

    header = (
        map(p -> merge(p_map, c_map)[p], vcat(ps, collect(keys(c_map)))),
        map(nameof ∘ typeof, vcat(ps, cs)),
        # incorrect order of compounds
        # [map(p -> "\"" * p.statement * "\"", ps)..., map(c -> map(c -> c.statement, primitives(c)), cs)...]
    )
    
    # cropping?
    pretty_table(
        interpretations,
        header = header,
        body_hlines = collect(0:2:2^length(ps)),
        crop = :none
    )
end
"""
    @truth_table(ps...)

Print a truth table for the given [`Compound`](@ref) propositions.

See also [`Primitive`](@ref) and [`Compound`](@ref).

# Examples
```jldoctest; setup = :(@primitive p q)
julia> @truth_table p∧q p→q
┌───────────┬───────────┬───────────────┬───────────────┐
│         p │         q │         p → q │         p ∧ q │
│ Primitive │ Primitive │ Propositional │ Propositional │
├───────────┼───────────┼───────────────┼───────────────┤
│         ⊤ │         ⊤ │             ⊤ │             ⊤ │
│         ⊤ │         ⊥ │             ⊥ │             ⊥ │
├───────────┼───────────┼───────────────┼───────────────┤
│         ⊥ │         ⊤ │             ⊤ │             ⊥ │
│         ⊥ │         ⊥ │             ⊤ │             ⊥ │
└───────────┴───────────┴───────────────┴───────────────┘
```
"""
macro truth_table(expressions...)
    f = expression -> typeof(expression) <: Union{Symbol, String} ? [expression] : mapreduce(f, vcat, expression.args[2:end])
    propositions = reduce(union, map(f, expressions))

    return :(
        truth_table(
            Propositional[$(map(esc, expressions)...)] .=> string.($expressions),
            [$(map(esc, propositions)...)] .=> string.($propositions)
        )
    )
end

"""
    equivalent(p::Language, q::Language)

Returns a boolean of whether propositions p and q are logically equivalent.

# Examples
```jldoctest; setup = :(@primitive p q)
julia>
```
"""
equivalent(p::Language, q::Language) = truth_set(p) == truth_set(q)










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
