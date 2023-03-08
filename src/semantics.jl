
import Base: ==, convert, promote_rule
import Base: reduce

"""
    p == q
    ==(p, q)
    isequal(p, q)

Returns a boolean indicating whether `p` and `q` are
[logically equivalent](https://en.wikipedia.org/wiki/Logical_equivalence).

!!! info
    The `≡` symbol is sometimes used to represent logical equivalence.
    However, Julia uses `≡` as an alias for the builtin function `===`
    which cannot have methods added to it.
    Use this function to test for identity rather than equivalence.

See also [`Proposition`](@ref).

# Examples
```
julia> @p p == ¬p
false

julia> @p (p → q) ∧ (p ← q) == ¬(p ⊻ q)
true

julia> @p (p → q) ∧ (p ← q) === ¬(p ⊻ q)
false
```
"""
==(p::NullaryOperator, q::NullaryOperator) = p === q
==(p::LiteralProposition, q::LiteralProposition) = Literal(p) === Literal(q)
==(p::Union{NullaryOperator, Proposition}, q::Union{NullaryOperator, Proposition}) = is_tautology(p ↔ q)

"""
    is_tautology(p)

Returns a boolean on whether `p` is a [`tautology`](@ref).

# Examples
```jldoctest
julia> is_tautology(⊤)
true

julia> @p is_tautology(p)
false

julia> @p is_tautology(¬(p ∧ ¬p))
true
```
"""
is_tautology(p::typeof(tautology)) = true
is_tautology(p::Union{typeof(⊥), Atom, Literal}) = false
is_tautology(p::CN) where CN <: Union{Clause{A}, Normal{A}} where A <: typeof(and) = isempty(getfield(p, 1))
is_tautology(p::Valuation) = all(isequal(tautology) ∘ last, p.interpretations)
is_tautology(p::Proposition) = is_tautology(Valuation(p))

"""
    is_contradiction(p)

Returns a boolean on whether `p` is a [`contradiction`](@ref).

# Examples
```jldoctest
julia> is_contradiction(⊥)
true

julia> @p is_contradiction(p)
false

julia> @p is_contradiction(p ∧ ¬p)
true
```
"""
is_contradiction(p) = is_tautology(¬p)

"""
    is_truth(p)

Returns a boolean on whether `p` is a truth value
(either a [`tautology`](@ref) or [`contradiction`](@ref)).

See also [`Proposition`](@ref).

# Examples
```jldoctest
julia> is_truth(⊤)
true

julia> @p is_truth(p ∧ ¬p)
true

julia> @p is_truth(p)
false

julia> @p is_truth(p ∧ q)
false
```
"""
is_truth(p::NullaryOperator) = true
is_truth(p::Union{Atom, Literal}) = false
is_truth(p::Clause) = isempty(p.literals) ? (return true) : return is_truth(Valuation(p))
is_truth(p::Valuation) = length(unique(map(last, p.interpretations))) == 1
is_truth(p::Proposition) = is_truth(Valuation(p))

"""
    is_contingency(p)

Returns a boolean on whether `p` is a contingency
(neither a [`tautology`](@ref) or [`contradiction`](@ref)).

See also [`Proposition`](@ref).

# Examples
```jldoctest
julia> is_contingency(⊤)
false

julia> @p is_contingency(p ∧ ¬p)
false

julia> @p is_contingency(p)
true

julia> @p is_contingency(p ∧ q)
true
```
"""
is_contingency(p) = !is_truth(p)

"""
    is_satisfiable(p)

Returns a boolean on whether `p` is satisfiable
(not a [`contradiction`](@ref)).

See also [`Proposition`](@ref).

# Examples
```jldoctest
julia> is_satisfiable(⊤)
true

julia> @p is_satisfiable(p ∧ ¬p)
false

julia> @p is_satisfiable(p)
true

julia> @p is_satisfiable(p ∧ q)
true
```
"""
is_satisfiable(p) = !is_contradiction(p)

"""
    is_falsifiable(p)

Returns a boolean on whether `p` is falsifiable
(not a [`tautology`](@ref)).

See also [`Proposition`](@ref).

# Examples
```jldoctest
julia> is_falsifiable(⊥)
true

julia> @p is_falsifiable(¬(p ∧ ¬p))
false

julia> @p is_falsifiable(p)
true

julia> @p is_falsifiable(p ∧ q)
true
```
"""
is_falsifiable(p) = !is_tautology(p)

"""
    converse(::BooleanOperator)

Returns the [`BooleanOperator`](@ref) that is the
[converse](https://en.wikipedia.org/wiki/Converse_(logic))
of the given boolean operator.

# Examples
```jldoctest
julia> converse(and)
and (generic function with 20 methods)

julia> @p and(p, q) == converse(and)(q, p)
true

julia> converse(imply)
converse_imply (generic function with 9 methods)

julia> @p imply(p, q) == converse(imply)(q, p)
true
```
"""
converse(::typeof(imply)) = converse_imply
converse(::typeof(not_imply)) = not_converse_imply
converse(::typeof(converse_imply)) = imply
converse(::typeof(not_converse_imply)) = not_imply
converse(::CO) where CO <: CommutativeOperator = CO.instance

"""
    dual(::BooleanOperator)

Returns the [`BooleanOperator`](@ref) that is the
[dual](https://en.wikipedia.org/wiki/Boolean_algebra#Duality_principle)
of the given boolean operator.

# Examples
```jldoctest
julia> dual(and)
or (generic function with 17 methods)

julia> @p and(p, q) == not(dual(and)(not(p), not(q)))
true

julia> dual(imply)
not_converse_imply (generic function with 9 methods)

julia> @p imply(p, q) == not(dual(imply)(not(p), not(q)))
true
```
"""
dual(::typeof(and)) = or
dual(::typeof(nand)) = nor
dual(::typeof(nor)) = nand
dual(::typeof(or)) = and
dual(::typeof(xor)) = xnor
dual(::typeof(xnor)) = xor
dual(::typeof(imply)) = not_converse_imply
dual(::typeof(not_imply)) = converse_imply
dual(::typeof(converse_imply)) = not_imply
dual(::typeof(not_converse_imply)) = imply
dual(::BO) where BO <: Union{map(typeof, [tautology, contradiction, xor, xnor])...} = not(BO.instance)
# TODO: `dual(::typeof(not))` and `dual(::typeof(identity))` ?

"""
    identity(::Symbol, ::BooleanOperator)

Given either `:left` or `:right` and a [`BooleanOperator`](@ref),
return the corresponding identity element, if it exists.

The identity element is either [`tautology`](@ref) or [`contradiction`](@ref).
If the identity element does not exist, throw an exception.

# Examples
```jldoctest
julia> identity(:right, or)
contradiction (generic function with 1 method)

julia> identity(:left, imply)
tautology (generic function with 1 method)
```
"""
identity(::Union{Val{:left}, Val{:right}}, ::Union{typeof(and), typeof(xnor)}) = ⊤
identity(::Union{Val{:left}, Val{:right}}, ::Union{typeof(or), typeof(xor)}) = ⊥
identity(::Val{:left}, ::typeof(imply)) = ⊤
identity(::Val{:right}, ::typeof(not_imply)) = ⊥
identity(::Val{:right}, ::typeof(converse_imply)) = ⊤
identity(::Val{:left}, ::typeof(not_converse_imply)) = ⊥
identity(x, bo::BinaryOperator) = identity(Val(x), bo)

"""
    interpret(p, valuation...)

Assign a truth value value to `p`.

Let `p` be a [`Proposition`](@ref).
Let `valuation` be a function, callable object, dictionary, or any number of `Pair`s
that map from [`atomic propositions`](@ref Atom) in `p` to their respective truth values.

Calling `p` with an incomplete mapping will partially interpret `p`.
This returns a `Proposition` of the same type as `p`?*?
that is independent from every `Atom`s in `valuation`.

!!! warning
    If `valuation` does not return a `Truth` or errors, 

# Examples
```

```
"""
interpret(p::Atom, valuation::Dict) = get(valuation, p, p)
interpret(p::Literal{UO}, valuation::Dict) where UO <: UnaryOperator = UO.instance(interpret(p.atom, valuation))
# interpret(p::Tree{BO}, valuation::Dict) where BO <: BooleanOperator = BO.instance(
#     map(p.p) do p
#         interpret(p, valuation)
#     end...
# )
function interpret(p::Clause{AO}, valuation::Dict) where AO <: AndOr
    neutral_element = identity(:left, AO.instance)
    isempty(p.literals) && return neutral_element

    interpretation = map(p.literals) do literal
        assignment = interpret(literal, valuation)
        assignment == not(neutral_element) && return not(neutral_element)
        assignment
    end
    return reduce(AO.instance, interpretation)

    # x = Literal[]
    # for atom in p.p
    #     assignment = interpret(atom, valuation)
    #     assignment == not(truth) && return not(truth)
    #     assignment == truth && continue
    #     push!(x, assignment)
    # end
    # return Clause(B.instance, interpretation)
end
# interpret(p::Normal)
# function interpret(p::Valuation, valuation::Dict)
#     interpretations = filter(p.p) do interpretation
#         last(interpretation) == ⊤
#     end
#     isempty(interpretations) && return ⊥

#     return mapreduce(or, interpretations) do interpretation
#         mapreduce(and, first(interpretation)) do pair
#             Dict(⊤ => identity, ⊥ => not)[last(pair)](first(pair))
#         end
#     end


    # new_interpretations = Pair{Vector{Pair{Atom, Truth}}, Truth}[]

    # for interpretation in p.p
    #     new_interpretation = Pair{Atom, Truth}[]
    #     _continue = false

    #     for pair in first(interpretation)
    #         x = interpret(first(pair), valuation) => last(pair)

    #         first(x) == last(x) && continue
    #         if first(x) == not(last(x))
    #             _continue = true
    #             continue
    #         end
    #         push!(new_interpretation, pair)
    #     end

    #     _continue || push!(new_interpretations, new_interpretation => last(interpretation))
    # end

    # new = Valuation(new_interpretations)
    # is_contradiction(new) && return ⊥
    # is_tautology(new) && return ⊤
    # return new
# end
interpret(p::Tree{BO}, valuation::Dict) where BO <: BooleanOperator = BO.instance(
    map(p.node) do branch
        interpret(branch, valuation)
    end...
)
# interpret(p::Proposition, valuation::Dict) = interpret(Normal(p), valuation)
interpret(p::Proposition, valuation::Dict) = interpret(Tree(p), valuation)
# interpret(p::Proposition, valuation::Dict) = interpret(Tree(p), valuation)
interpret(p::Proposition, valuation...) = interpret(p, Dict(valuation))
# interpret(p::Proposition, valuation::Vector{Pair}) = interpret(p, Dict(valuation))

"""
    solve(p)

Return a vector of every valid interpretation of `p`.

# Examples
```jldoctest
julia> @p solve(p ⊻ q)
2-element Vector{Vector{Pair{Atom{Symbol}}}}:
 [p => ⊥, q => ⊤]
 [p => ⊤, q => ⊥]
```
"""
solve(p::Valuation) = map(
    first,
    filter(p.interpretations) do interpretation
        last(interpretation) == ⊤
    end
)
solve(p) = solve(Valuation(p))
# solve(p, qs) = solve(reduce(and, qs, init = p))
# solve(p, qs...) = solve(p, collect(qs))

"""
    (p::Proposition)(valuation...)

Equivalent to [`interpret`](@ref), except guaranteed to return the same type.

# Examples
"""
# (p::Union{Clause{AO}, Normal{AO}})(valuation...) where AO <: Union{typeof(and), typeof(or)} = nameof(typeof(p))(AO.instance, interpret(p, Dict(valuation)))
(p::Proposition)(valuation...) = getfield(Main, nameof(typeof(p)))(interpret(p, Dict(valuation)))

# TODO: write all conversions
# TODO: if simplification is about the operator, put it with operators
#    if it's about the types, put it in constructors/convert

# Boolean Operators

# generic
foreach([tautology, contradiction]) do truth
    @eval $(Symbol(truth))() = $truth
end
nand(p, q) = ¬(p ∧ q)
nor(p, q) = ¬p ∧ ¬q
or(p, q) = ¬(p ⊽ q)
xor(p, q) = (p ∨ q) ∧ (p ⊼ q)
xnor(p, q) = (p → q) ∧ (p ← q)
not_imply(p, q) = p ∧ ¬q
imply(p, q) = ¬(p ↛ q)
not_converse_imply(p, q) = ¬p ∧ q
converse_imply(p, q) = ¬(p ↚ q)

# Bool
not(p::Bool) = !p
and(p::Bool, q::Bool) = p && q
or(p::Bool, q::Bool) = p || q

# boolean operators
not(::typeof(contradiction)) = tautology
not(::typeof(tautology)) = contradiction
not(::typeof(identity)) = not
not(::typeof(not)) = identity
not(::typeof(and)) = nand
not(::typeof(nand)) = and
not(::typeof(nor)) = or
not(::typeof(or)) = nor
not(::typeof(xor)) = xnor
not(::typeof(xnor)) = xor
not(::typeof(imply)) = not_imply
not(::typeof(not_imply)) = imply
not(::typeof(converse_imply)) = not_converse_imply
not(::typeof(not_converse_imply)) = converse_imply

# propositions
not(p::Atom) = Literal(not, p)
not(p::Literal{UO}) where UO <: UnaryOperator = not(UO.instance)(p.atom)
not(p::CN) where CN <: Union{Clause{AO}, Normal{AO}} where AO <: AndOr = getfield(Main, nameof(CN))(
    dual(AO.instance), map(not, getfield(p, 1))
)
not(p::Valuation) = Valuation(
    map(p.interpretations) do interpretation # Pair{Vector{Pair{Atom, Truth}}, Truth}
        first(interpretation) => not(last(interpretation))
    end
)
not(p::Tree{BO}) where BO <: BooleanOperator = not(BO.instance)(p.node...)

and(::typeof(tautology), ::typeof(tautology)) = ⊤
and(::typeof(contradiction), ::Union{NullaryOperator, Proposition}) = ⊥ # domination law
and(::typeof(tautology), q::Union{NullaryOperator, Proposition}) = q # identity law
and(p::Proposition, q::NullaryOperator) = q ∧ p # commutative property
# and(p::Valuation, q::Valuation)

foreach(Base.uniontypes(AndOr)) do AO
    ao = Symbol(AO.instance)
    dao = Symbol(dual(AO.instance))
    DAO = typeof(dual(AO.instance))

    foreach([Clause, Normal]) do CN
        @eval $ao(p::$CN{$AO}, q::$CN{$AO}) = $CN($ao, vcat(getfield(p, 1), getfield(q, 1)))
    end
    @eval $ao(p::Clause{$DAO}, q::Clause{$DAO}) = Normal($ao, p, q)
    @eval $ao(p::Normal, q::Normal)= $ao(Normal($ao, p), Normal($ao, q))

    @eval $ao(p::LiteralProposition, q::Clause{AO}) where AO <: typeof($ao) = Clause($ao, vcat(p, q.literals))
    @eval $ao(p::Clause{AO}, q::LiteralProposition) where AO <: typeof($ao) = Clause($ao, vcat(p.literals, q))
end
# or(p::Valuation, q::Valuation) = Valuation(vcat(p.p, q.p))

not(p::AbstractArray) = map(not, p)

foreach(Base.uniontypes(BinaryOperator)) do BO
    bo = Symbol(BO.instance)

    @eval $bo(p) = Base.Fix1($bo, p)

    @eval $bo(p::Proposition, q::AbstractArray) = map($bo(p), q)
    @eval $bo(p::AbstractArray, q::Proposition) = map($bo(q), p)
    # @eval function $bo(p::VecOrMat, q::VecOrMat)

    # end
end

# generic fallbacks and promotion
foreach(Base.uniontypes(BinaryOperator)) do BO
    bo = Symbol(BO.instance)

    foreach([Atom, Literal]) do AL
        @eval $bo(p::$AL, q::$AL) = $bo(Tree(p), Tree(q))
    end
    @eval $bo(p::Tree, q::Tree) = Tree($bo, p, q)
    @eval $bo(p::Union{Atom, Literal}, q::Tree) = Tree($bo, Tree(p), q)
    @eval $bo(p::Tree, q::Union{Atom, Literal}) = Tree($bo, p, Tree(q))
end
and(p::Proposition, q::Proposition) = and(promote(p, q)...)
or(p::Union{Clause, Normal}, q::Union{Clause, Normal}) = or(promote(p, q)...)

# Constructors

function Clause(::AO, ps::AbstractArray) where AO <: AndOr
    neutral_element = identity(:left, AO.instance)
    qs = Literal[]
    for p in ps
        if p isa NullaryOperator
            p == neutral_element && continue
            r = Atom()
            return Clause(AO.instance, [r, ¬r])
        elseif p isa Clause{AO}
            append!(qs, p.literals)
        else
            push!(qs, p)
        end
    end
    return Clause(AO.instance, qs)
end
Clause(::AO, ps...) where AO <: AndOr = Clause(AO.instance, collect(ps))

Normal(::AO, p::Tree{BO}) where {AO <: AndOr, BO <: BooleanOperator} = BO.instance(
    map(p.node) do branch
        Normal(AO.instance, branch)
    end...
)
# Normal(::AO, ps::AbstractArray) where AO <: AndOr
# Normal(::AO, p::Normal) where AO <: AndOr = Normal(
#     AO.instance,
#     foldl(distribute, p.p, init = Clause{typeof(dual(AO.instance))}[])
# )
Normal(::AO, p::Normal{AO}) where AO <: AndOr = p
Normal(::AO, ps...) where AO <: AndOr = Normal(AO.instance, collect(ps))

# function distribute(ps::Vector{<:Clause}, q::Clause{AO}) where AO <: AndOr
#     return map(q.p) do qp
#         Clause(
#             dual(AO.instance),
#             vcat(
#                 qp, map(ps)
#             )
#         )
#     end

#     isempty(ps) && return [q]
#     xs = Clause{typeof(dual(AO.instance))}[]
#     for p in ps
#         push!(xs, Clause(dual(AO.instance), vcat(p.p, q.p)))
#     end
#     return xs
# end
foreach([Atom, Literal, Clause, Normal, Valuation, Tree]) do P
    @eval $(nameof(P))(p) = convert($(nameof(P)), p)
end

# Promotion

"""
    promote_rule
"""
promote_rule(::Type{<:Atom}, ::Type{<:Atom}) = Atom
promote_rule(::Type{<:Atom}, ::Type{<:Literal}) = Literal
# promote_rule(::Type{<:Union{Atom, Literal}}, ::Type{<:Clause}) = Clause
# promote_rule(::Type{<:Clause{AO}}, ::Type{<:Clause{AO}}) where AO <: AndOr = Clause
# promote_rule(::Type{<:Clause{<:AndOr}}, ::Type{<:Clause{<:AndOr}}) = Normal
# foreach(get_concrete_types(Expressive)) do type
#     @eval promote_rule(::Type{<:Proposition}, ::Type{<:$type}) = $type
# end
# foreach(setdiff(concrete_propositions, [Clause])) do type
#     @eval promote_rule(::Type{<:$type}, ::Type{<:$type}) = $type
# end
promote_rule(::Type{<:Proposition}, ::Type{<:Proposition}) = Tree # generic fallback

# Conversions

function get_valuations(atoms)
    n = length(atoms)
    truth_sets = Iterators.product(Iterators.repeated([⊤, ⊥], n)...)
    return vec(
        map(truth_sets) do truth_set # Vector{Truth}
            map(Pair, atoms, truth_set)
        end
    )
end

get_interpretation(p, valuations) = map(valuations) do valuation
    interpret(p, valuation)
end

"""
    convert
"""
convert(::Type{Atom}, p::Literal{typeof(identity)}) = p.atom
convert(::Type{Atom}, p::Tree{typeof(identity), <:Tuple{Atom}}) = only(p.node)
convert(::Type{Literal}, p::Tree{UO, <:Tuple{Atom}}) where UO <: UnaryOperator = ¬(¬p)
convert(::Type{LT}, p::Atom) where LT <: Union{Literal, Tree} = LT(identity, p)
convert(::Type{Clause}, p::typeof(tautology)) = Clause(and)
convert(::Type{Clause}, p::typeof(contradiction)) = Clause(or)
function convert(::Type{Tree}, p::typeof(contradiction))
    p = Atom()
    return p ∧ ¬p
end
convert(::Type{Tree}, p::typeof(tautology)) = not(Tree(contradiction))
convert(::Type{Tree}, p::Literal{UO}) where UO <: UnaryOperator = Tree(UO.instance, p.atom)
function convert(::Type{Tree}, p::Valuation)
    is_truth(p) && return Tree(last(first(p.interpretations)))

    valid_interpretations = filter(isequal(tautology) ∘ last, p.interpretations)
    pair_to_literal = pair -> (last(pair) == tautology ? identity : not)(Literal(first(pair)))
    mapreduce_and = interpretation -> mapreduce(pair_to_literal, and, first(interpretation))
    mapreduce_or = interpretations -> mapreduce(mapreduce_and, or, interpretations)
    return Tree(mapreduce_or(valid_interpretations))
end
convert(::Type{Tree}, p::Clause{AO}) where AO <: AndOr = reduce(AO.instance, p.literals)
convert(::Type{Tree}, p::Normal{AO}) where AO <: AndOr = mapreduce(Tree, AO.instance, p.clauses)
convert(::Type{Valuation}, ::NO) where NO <: NullaryOperator = Valuation([[] => NO.instance])
function convert(::Type{Valuation}, p::Union{setdiff(concrete_propositions, [Valuation])...})
    valuations = get_valuations(get_atoms(p))
    interpretation = get_interpretation(p, map(Dict, valuations))

    # return Valuation(vec(map(Pair{Vector{Pair{Atom, Truth}}, Truth}, valuations, interpretation)))
    return Valuation(vec(map(Pair, valuations, interpretation)))
end




# dynamically generate? refactor?
# Tree(p::Proposition) = convert(Tree, p)
# Clause(::B, p::Proposition) where B <: Union{typeof(and), typeof(or)} = convert(Clause{B}, p)
# Normal(::AO, p::Proposition) where AO <: Union{typeof(and), typeof(or)} = convert(Normal{AO}, p)

# convert(::Type{Normal{typeof(and)}}, p::Proposition) = not(Normal(or, ¬p))
# function convert(::Type{Normal{typeof(or)}}, p::Proposition)
#     q = Valuation(p)

#     is_tautology(q) && return Normal(or, Clause(and))

#     # TODO: change `===` to `==` - fixes `Normal(and, ⊥)`
#     interpretations =
#         if is_contradiction(q)
#             atom = Atom()
#             [[atom => ⊤, atom => ⊥]]
#         else
#             map(
#                 first,
#                 filter(
#                     literal -> last(literal) == ⊤,
#                     q.p
#                 )
#             )
#         end

#     clauses = map(
#         interpretation -> Clause(
#             and,
#             map(
#                 pair -> last(pair) == ⊤ ? Literal(first(pair)) : not(first(pair)),
#                 interpretation
#             )
#         ),
#         interpretations
#     )

#     return Normal(or, clauses)
# end
