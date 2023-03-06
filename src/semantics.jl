
import Base: ==, identity, convert, promote_rule
import Base: reduce

"""
    p == q
    ==(::Proposition, ::Proposition)
    isequal(::Proposition, ::Proposition)

Returns a boolean indicating whether ```p``` and ```q``` are
[logically equivalent](https://en.wikipedia.org/wiki/Logical_equivalence).

!!! info
    The ```≡``` symbol is sometimes used to represent logical equivalence.
    However, Julia uses ```≡``` as an alias for the builtin function ```===```
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
==(p::Union{literal_propositions...}, q::Union{literal_propositions...}) = Literal(p) === Literal(q)
==(p::Union{NullaryOperator, Proposition}, q::Union{NullaryOperator, Proposition}) = is_tautology(p ↔ q)

"""
    is_tautology(::Proposition)

Returns a boolean on whether the given proposition is a [`tautology`](@ref).

This function is equivalent to ```==(⊤)```.

See also [`Proposition`](@ref) and [`==`](@ref).

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
is_tautology(p::CN) where CN <: Union{Clause{A}, Normal{A}} where A <: typeof(and) = isempty(p.p)
is_tautology(p::Valuation) = all(isequal(tautology) ∘ last, p.p)
is_tautology(p::Proposition) = is_tautology(Valuation(p))

"""
    is_contradiction(::Proposition)

Returns a boolean on whether the given proposition is a [`contradiction`](@ref).

This function is equivalent to ```==(⊥)```.

See also [`Proposition`](@ref) and [`==`](@ref).

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
    is_truth(::Proposition)

Returns a boolean on whether the given proposition is a truth value
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
is_truth(p::Clause) = isempty(p.p) ? (return true) : return is_truth(Valuation(p))
is_truth(p::Valuation) = length(unique(map(last, p.p))) == 1
is_truth(p::Proposition) = is_truth(Valuation(p))

"""
    is_contingency(::Proposition)

Returns a boolean on whether the given proposition is a contingency
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
    is_satisfiable(::Proposition)

Returns a boolean on whether the given proposition is satisfiable
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
    is_falsifiable(::Proposition)

Returns a boolean on whether the given proposition is falsifiable
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
    is_commutative(::BooleanOperator)

Returns a boolean on whether the given [`BooleanOperator`](@ref) has the
[commutative property](https://en.wikipedia.org/wiki/Commutative_property)

# Examples
```jldoctest
julia> is_commutative(and)
true

julia> @p p ∧ q == q ∧ p
true

julia> is_commutative(imply)
false

julia> @p (p → q) == (q → p)
false
```
"""
is_commutative(::Union{
    typeof(and),
    typeof(nand),
    typeof(nor),
    typeof(or),
    typeof(xor),
    typeof(xnor)
}) = true
is_commutative(::BinaryOperator) = false

"""
    is_associative(::BooleanOperator)

Returns a boolean on whether the given [`BooleanOperator`] has the 
[associative property](https://en.wikipedia.org/wiki/Associative_property).

# Examples
```jldoctest
julia> is_associative(and)
true

julia> @p (p ∧ q) ∧ r == p ∧ (q ∧ r)
true

julia> is_associative(nand)
false

julia> @p (p ⊼ q) ⊼ r == p ⊼ (q ⊼ r)
false
```
"""
is_associative(::Union{
    typeof(and),
    typeof(or),
    typeof(xor),
    typeof(xnor)
}) = true
is_associative(::BinaryOperator) = false

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
converse(::BO) where BO <: Union{
    typeof(and),
    typeof(nand),
    typeof(nor),
    typeof(or),
    typeof(xor),
    typeof(xnor)
} = BO.instance

"""
    dual(::BooleanOperator)

Returns the [`BooleanOperator`](@ref) that is the
[dual](https://en.wikipedia.org/wiki/Boolean_algebra#Duality_principle)
of the given boolean operator.

# Examples
```jldocttest
julia> dual(and)
or (generic function with 20 methods)

julia> @p and(p, q) == not(dual(and)(not(p), not(q)))
true

julia> dual(imply)
not_converse_imply (generic function with 12 methods)

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
dual(::BO) where BO <: Union{
    typeof(tautology),
    typeof(contradiction),
    typeof(xor),
    typeof(xnor)
} = not(BO.instance)

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
    interpret(p::Proposition, valuation...)

Assign a truth value value to ```p```.

Let ```p``` be a [`Proposition`](@ref).
Let ```valuation``` be a function, callable object, dictionary, or any number of ```Pair```s
that map from [`atomic propositions`](@ref Atom) in ```p``` to their respective truth values.

Calling ```p``` with an incomplete mapping will partially interpret ```p```.
This returns a ```Proposition``` of the ?*?same type as ```p```?*?
that is independent from every ```Atom```s in ```valuation```.

!!! warning
    If ```valuation``` does not return a ```Truth``` or errors, 

# Examples
```

```
"""
interpret(p::Atom, valuation::Dict) = get(valuation, p, p)
interpret(p::Literal{UO}, valuation::Dict) where UO <: UnaryOperator = UO.instance(interpret(p.p, valuation))
# interpret(p::Tree{BO}, valuation::Dict) where BO <: BooleanOperator = BO.instance(
#     map(p.p) do p
#         interpret(p, valuation)
#     end...
# )
function interpret(p::Clause{B}, valuation::Dict) where B <: Union{typeof(and), typeof(or)}
    neutral_element = identity(:left, B.instance)
    isempty(p.p) && return neutral_element

    interpretation = map(p.p) do atom
        assignment = interpret(atom, valuation)
        assignment == not(neutral_element) && return not(neutral_element)
        assignment
    end
    return reduce(B.instance, interpretation)

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
    map(p.p) do q
        interpret(q, valuation)
    end...
)
# interpret(p::Proposition, valuation::Dict) = interpret(Normal(p), valuation)
interpret(p::Proposition, valuation::Dict) = interpret(Tree(p), valuation)
# interpret(p::Proposition, valuation::Dict) = interpret(Tree(p), valuation)
interpret(p::Proposition, valuation...) = interpret(p, Dict(valuation))
# interpret(p::Proposition, valuation::Vector{Pair}) = interpret(p, Dict(valuation))

"""
    solve(p::Proposition)

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
    filter(p.p) do interpretation
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
left(p, q) = p
not_left(p, q) = ¬p
right(p, q) = q
not_right(p, q) = ¬q
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
not(::typeof(left)) = not_left
not(::typeof(not_left)) = left
not(::typeof(right)) = not_right
not(::typeof(not_right)) = right
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
not(p::Literal{UO}) where UO <: UnaryOperator = not(UO.instance)(p.p)
not(p::CN) where CN <: Union{Clause{AO}, Normal{AO}} where AO <: AndOr = getfield(Main, nameof(CN))(
    dual(AO.instance), map(not, p.p)
)
not(p::Valuation) = Valuation(
    map(p.p) do interpretation # Pair{Vector{Pair{Atom, Truth}}, Truth}
        first(interpretation) => not(last(interpretation))
    end
)
not(p::Tree{BO}) where BO <: BooleanOperator = not(BO.instance)(p.p...)

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
        @eval $ao(p::$CN{$AO}, q::$CN{$AO}) = $CN($ao, vcat(p.p, q.p))
    end
    @eval $ao(p::Clause{$DAO}, q::Clause{$DAO}) = Normal($ao, p, q)
    @eval $ao(p::Normal, q::Normal)= $ao(Normal($ao, p), Normal($ao, q))

    @eval $ao(p::Union{literal_propositions...}, q::Clause{AO}) where AO <: typeof($ao) = Clause($ao, vcat(p, q.p))
    @eval $ao(p::Clause{AO}, q::Union{literal_propositions...}) where AO <: typeof($ao) = Clause($ao, vcat(p.p, q))
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
foreach([
    typeof(and),
    typeof(nand),
    typeof(nor),
    typeof(or),
    typeof(xor),
    typeof(xnor),
    typeof(imply),
    typeof(not_imply),
    typeof(converse_imply),
    typeof(not_converse_imply)
]) do BO
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

# Promotion

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
            append!(qs, p.p)
        else
            push!(qs, p)
        end
    end
    return Clause(AO.instance, qs)
end
Clause(::AO, ps...) where AO <: AndOr = Clause(AO.instance, collect(ps))

Normal(::AO, p::Tree{BO}) where {AO <: AndOr, BO <: BooleanOperator} = BO.instance(
    map(p.p) do p
        Normal(AO.instance, p)
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

convert(::Type{Atom}, p::Literal{typeof(identity)}) = p.p
convert(::Type{Atom}, p::Tree{typeof(identity), <:Tuple{Atom}}) = only(p.p)
convert(::Type{Literal}, p::Tree{UO, <:Tuple{Atom}}) where UO <: UnaryOperator = ¬(¬p)
convert(::Type{LT}, p::Atom) where LT <: Union{Literal, Tree} = LT(identity, p)
convert(::Type{Clause}, p::typeof(tautology)) = Clause(and)
convert(::Type{Clause}, p::typeof(contradiction)) = Clause(or)
function convert(::Type{Tree}, p::typeof(contradiction))
    p = Atom()
    return p ∧ ¬p
end
convert(::Type{Tree}, p::typeof(tautology)) = not(Tree(contradiction))
convert(::Type{Tree}, p::Literal{UO}) where UO <: UnaryOperator = Tree(UO.instance, p.p)
function convert(::Type{Tree}, p::Valuation)
    is_truth(p) && return Tree(last(first(p.p)))

    valid_interpretations = filter(isequal(tautology) ∘ last, p.p)
    pair_to_literal = pair -> (last(pair) == tautology ? identity : not)(Literal(first(pair)))
    mapreduce_and = interpretation -> mapreduce(pair_to_literal, and, first(interpretation))
    mapreduce_or = interpretations -> mapreduce(mapreduce_and, or, interpretations)
    return Tree(mapreduce_or(valid_interpretations))
end
convert(::Type{Tree}, p::Clause{AO}) where AO <: AndOr = reduce(AO.instance, p.p)
convert(::Type{Tree}, p::Normal{AO}) where AO <: AndOr = mapreduce(Tree, AO.instance, p.p)
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
