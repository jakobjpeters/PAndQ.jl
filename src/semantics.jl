
import Base: ==, Fix1, convert, Bool

"""
    p == q
    ==(p, q)

Returns a boolean indicating whether `p` and `q` are [logically equivalent]
(https://en.wikipedia.org/wiki/Logical_equivalence).

!!! info
    The `≡` symbol is sometimes used to represent logical equivalence.
    However, Julia uses `≡` as an alias for the builtin function `===`
    which cannot have methods added to it.
    Use `==` and `===` to test for equivalence and identity, respectively.

See also [`Proposition`](@ref).

# Examples
```
julia> @p p == ¬p
false

julia> @p ¬(p ⊻ q) == (p → q) ∧ (p ← q)
true

julia> @p ¬(p ⊻ q) === (p → q) ∧ (p ← q)
false
```
"""
==(p::NullaryOperator, q::NullaryOperator) = p === q
==(p::Union{NullaryOperator, Proposition}, q::Union{NullaryOperator, Proposition}) =
    is_tautology(p ↔ q)

"""
    interpret(p::Union{NullaryOperator, Proposition}, valuation...)

Replaces each [`Atom`](@ref) in `p` with its truth value in `valuation`,
then simplifies.

`valuation` is either a `Dict` or a set that can construct one
that maps from atoms to their respective truth values.

Calling `p` with an incomplete mapping will partially interpret it.

See also [`tautology`](@ref) and [`contradiction`].

# Examples
```
julia> @p interpret(¬p, p => ⊤)
contradiction (generic function with 1 method)

julia> @p p = Clause(and, q, r, s)
Clause:
 q ∧ r ∧ s

julia> @p interpret(p, q => ⊤, r => ⊤)
Clause:
 s
```
"""
interpret(p::NullaryOperator, valuation::Dict) = p
interpret(p::Atom, valuation::Dict) = get(valuation, p, p)
interpret(p::Literal{UO}, valuation::Dict) where UO <: UnaryOperator =
    interpret(p.atom, valuation) |> UO.instance
function interpret(p::CN, valuation::Dict) where {AO <: AndOr, CN <: Union{Clause{AO}, Normal{AO}}}
    neutral_element = identity(:left, AO.instance)
    not_neutral_element = neutral_element |> not
    q = AO.instance |> getfield(Main, CN |> nameof)

    for r in getfield(p, 1)
        s = interpret(r, valuation)
        s == not_neutral_element && return not_neutral_element
        q = AO.instance(q, s)
    end

    getfield(q, 1) |> isempty ? neutral_element : q
end
interpret(p::Proposition, valuation::Dict) = interpret(Normal(and, p), valuation)
interpret(p, valuation) = interpret(p, valuation |> Dict)
interpret(p, valuation...) = interpret(p, valuation)

"""
    (p::Proposition)(valuation...)

Equivalent to [`interpret(p, valuation)`](@ref interpret) for all [`Proposition`](@ref)s.

# Examples
```jldoctest
julia> @p (¬p)(p => ⊤)
contradiction (generic function with 1 method)

julia> @p p = Clause(and, q, r, s)
Clause:
 q ∧ r ∧ s

julia> @p p(q => ⊤, r => ⊤)
Clause:
 s
```
"""
(p::Proposition)(valuation::Dict) = interpret(p, valuation)
(p::Proposition)(valuation...) = interpret(p, valuation)

"""
    valuations(atoms)
    valuations(::Proposition)

Return a `Vector`` containing every possible valuation of the [`Atom`](@ref)s.

A valuation is a vector of `Pair`s which map from an atom to a truth value.

# Examples
```jldoctest
julia> @p valuations([p])
2-element Vector{Vector}:
 Pair{Atom{Symbol}, typeof(tautology)}[p => ⊤]
 Pair{Atom{Symbol}, typeof(contradiction)}[p => ⊥]

julia> @p valuations([p, q])
4-element Vector{Vector}:
 Pair{Atom{Symbol}, typeof(tautology)}[p => ⊤, q => ⊤]
 Pair{Atom{Symbol}}[p => ⊥, q => ⊤]
 Pair{Atom{Symbol}}[p => ⊤, q => ⊥]
 Pair{Atom{Symbol}, typeof(contradiction)}[p => ⊥, q => ⊥]
```
"""
function valuations(atoms)
    n = atoms |> length
    return map(0:2 ^ n - 1) do i
        map(zip(atoms, digits(i, base = 2, pad = n))) do (left, right)
            left => right == 0 ? tautology : contradiction
        end
    end
end
valuations(p::Proposition) = p |> atoms |> valuations
valuations(x::NullaryOperator) = [x => x]

"""
    interpretations(p, valuations = valuations(p))

Return a vector of values given by [`interpret`](@ref)ing `p` each valuation.

See also [`valuations`](@ref).

# Examples
```jldoctest
julia> @p interpretations(p)
2-element Vector{Function}:
 tautology (generic function with 1 method)
 contradiction (generic function with 1 method)

julia> @p interpretations(p → q, [p => ⊤])
1-element Vector{Normal{typeof(or), Clause{typeof(and)}}}:
 (q)
```
"""
interpretations(p, valuations = valuations(p)) = map(valuations) do valuation
    interpret(p, valuation)
end

"""
    solve(p, truth_value = ⊤)

Return a vector containing all [`interpretations`](@ref) such that
`interpret(p, interpretation) == truth_value`.

# Examples
```jldoctest
julia> @p solve(p)
1-element Vector{Vector{Pair{Atom{Symbol}, typeof(tautology)}}}:
 [p => ⊤]

julia> @p solve(p ⊻ q, ⊥)
2-element Vector{Vector}:
 Pair{Atom{Symbol}, typeof(tautology)}[p => ⊤, q => ⊤]
 Pair{Atom{Symbol}, typeof(contradiction)}[p => ⊥, q => ⊥]
```
"""
function solve(p, truth_value = ⊤)
    _valuations = p |> valuations
    _interpretations = interpretations(p, _valuations)
    return map(filter(zip(_valuations, _interpretations) |> collect) do (valuation, interpretation)
        interpretation == truth_value
    end) do (valuation, interpretation)
        valuation
    end
end

"""
    identity(::Symbol, ::BooleanOperator)

Given either `:left` or `:right` and a [`BooleanOperator`](@ref),
return the corresponding identity element, if it exists.

The identity element is either [`tautology`](@ref) or [`contradiction`](@ref).
Throws an exception if the identity element does not exist.

# Examples
```jldoctest
julia> identity(:right, or)
contradiction (generic function with 1 method)

julia> identity(:left, imply)
tautology (generic function with 1 method)
```
"""
identity(x, binary_operator::BinaryOperator) = identity(x |> Val, binary_operator)
foreach([(:and, :xnor, :⊤), (:or, :xor, :⊥)]) do (left, middle, right)
    @eval identity(::Union{Val{:left}, Val{:right}}, ::union_typeof([$left, $middle])) = $right
end
foreach([
    (:left, :imply, :⊤),
    (:right, :not_imply, :⊥),
    (:right, :converse_imply, :⊤),
    (:left, :not_converse_imply, :⊥)
]) do (left, middle, right)
    @eval identity(::$(typeof(left |> Val)), ::typeof($middle)) = $right
end

eval_doubles(f, doubles) = foreach(doubles) do double
    foreach([double, double |> reverse]) do (left, right)
        @eval $f(::typeof($left)) = $right
    end
end

"""
    converse(::BooleanOperator)

Returns the [`BooleanOperator`](@ref) that is the
[converse](https://en.wikipedia.org/wiki/Converse_(logic))
of the given boolean operator.

# Examples
```jldoctest
julia> converse(and)
and (generic function with 23 methods)

julia> @p and(p, q) == converse(and)(q, p)
true

julia> converse(imply)
converse_imply (generic function with 7 methods)

julia> @p imply(p, q) == converse(imply)(q, p)
true
```
"""
converse(::CO) where CO <: CommutativeOperator = CO.instance
eval_doubles(:converse, [(imply, converse_imply), (not_imply, not_converse_imply)])

"""
    dual(::BooleanOperator)

Returns the [`BooleanOperator`](@ref) that is the
[dual](https://en.wikipedia.org/wiki/Boolean_algebra#Duality_principle)
of the given boolean operator.

# Examples
```jldoctest
julia> dual(and)
or (generic function with 19 methods)

julia> @p and(p, q) == not(dual(and)(not(p), not(q)))
true

julia> dual(imply)
not_converse_imply (generic function with 6 methods)

julia> @p imply(p, q) == not(dual(imply)(not(p), not(q)))
true
```
"""
dual(::BO) where BO <: [tautology, contradiction, xor, xnor] |> union_typeof =
    BO.instance |> not
eval_doubles(:dual, [
    (and, or),
    (nand, nor),
    (xor, xnor),
    (imply, not_converse_imply),
    (not_imply, converse_imply)
])
# TODO: `dual(::typeof(not))` and `dual(::typeof(identity))` ?

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
is_tautology(p) = all(Fix1(==, ⊤), p |> interpretations)
is_tautology(p::CN) where {A <: typeof(and), CN <: Union{Clause{A}, Normal{A}}} =
    getfield(p, 1) |> isempty

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
is_contradiction(p) = ¬p |> is_tautology

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
is_truth(p) = p |> interpretations |> unique! |> length == 1

"""
    is_contingency(p)

Returns a boolean on whether `p` is a
[contingency](https://en.wikipedia.org/wiki/Contingency_(philosophy))
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
is_contingency(p) = p |> !is_truth

"""
    is_satisfiable(p)

Returns a boolean on whether `p` is
[satisfiable](https://en.wikipedia.org/wiki/Satisfiability)
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
is_satisfiable(p) = p |> !is_contradiction

"""
    is_falsifiable(p)

Returns a boolean on whether `p` is
[falsifiable](https://en.wikipedia.org/wiki/Falsifiability)
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
is_falsifiable(p) = p |> !is_tautology

# TODO: write all conversions
# TODO: if simplification is about the operator, put it with operators
#    if it's about the types, put it in constructors/convert

# Boolean Operators

# generic
foreach([:tautology, :contradiction]) do truth
    @eval $truth() = $truth
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
foreach([:and, :or]) do and_or
    @eval $(Symbol(:reduce_, and_or))(xs) = reduce($and_or, xs)
end

# boolean operators
eval_doubles(:not, [
    (tautology, contradiction),
    (identity, not),
    (and, nand),
    (or, nor),
    (xor, xnor),
    (imply, not_imply),
    (converse_imply, not_converse_imply)
])

# propositions
not(p::Atom) = Literal(not, p)
not(p::Literal{UO}) where UO <: UnaryOperator = p.atom |> not(UO.instance)
not(p::Tree{BO}) where BO <: BooleanOperator = not(BO.instance)(p.node...)
not(p::CN) where CN <: Union{Clause{AO}, Normal{AO}} where AO <: AndOr =
    getfield(Main, CN |> nameof)(AO.instance |> dual, map(not, getfield(p, 1)))

and(::typeof(tautology), ::typeof(tautology)) = ⊤
and(::typeof(contradiction), ::Union{NullaryOperator, Proposition}) = ⊥ # domination law
and(::typeof(tautology), q::Union{NullaryOperator, Proposition}) = q # identity law
and(p::Proposition, q::NullaryOperator) = q ∧ p # commutative property
and(p::Proposition, q::Proposition) = and(Normal(and, p), Normal(and, q))

foreach(BinaryOperator |> uniontypes) do BO
    binary_operator = BO.instance |> Symbol
    @eval $binary_operator(p) = Fix1($binary_operator, p)
    @eval $binary_operator(p::Tree, q::Tree) = Tree($binary_operator, p, q)
    @eval $binary_operator(p::Tree, q::Proposition) = Tree($binary_operator, p, q |> Tree)
    @eval $binary_operator(p::Proposition, q::Tree) = Tree($binary_operator, p |> Tree, q)
    @eval $binary_operator(p::Union{Atom, Literal}, q::Union{Atom, Literal}) =
        $binary_operator(p |> Tree, q |> Tree)
end

foreach(AndOr |> uniontypes) do AO
    ao = AO.instance |> Symbol
    dao = AO.instance |> dual |> Symbol
    DAO = AO.instance |> dual |> typeof

    @eval $ao(p::Clause{$DAO}, q::Clause{$DAO}) = Normal($ao, p, q)

    @eval $ao(p::Union{LiteralProposition, Clause{$AO}}, q::Clause{$DAO}) =
        $ao(Normal($ao, p), q)
    @eval $ao(p::Clause{$DAO}, q::Union{LiteralProposition, Clause{$AO}}) =
        $ao(p, Normal($ao, q))

    foreach([(Clause, LiteralProposition), (Normal, Clause{DAO})]) do (left, right)
        @eval $ao(p::$left{$AO}, q::$right) = $left($ao, vcat(getfield(p, 1), q))
        @eval $ao(p::$right, q::$left{$AO}) = $left($ao, vcat(p, getfield(q, 1)))
    end

    foreach([Clause, Normal]) do ClauseNormal
        @eval $ao(p::$ClauseNormal{$AO}, q::$ClauseNormal{$AO}) =
            $ClauseNormal($ao, vcat(getfield(p, 1), getfield(q, 1)))
    end

    @eval $ao(p::Normal, q::Normal) = $ao(Normal($ao, p), Normal($ao, q))
    @eval $ao(p::Clause, q::Normal) = $ao(Normal($ao, p), q)
    @eval $ao(p::Normal, q::Clause) = $ao(p, Normal($ao, q))
end

# Constructors

Clause(::AO, ps::AbstractArray) where AO <: AndOr =
    ps |> isempty ? AO.instance |> Clause : Clause(AO.instance, map(Literal, ps))
Clause(::AO, ps...) where AO <: AndOr = Clause(AO.instance, ps |> collect)

Normal(::AO, p::Tree{BO}) where {AO <: AndOr, BO <: BooleanOperator} = BO.instance(
    map(p.node) do branch
        Normal(AO.instance, branch)
    end...
)
Normal(::AO, p::Clause{AO}) where AO <: AndOr = Normal(AO.instance, map(p.literals) do literal
    Clause(AO.instance |> dual, literal)
end)
Normal(::AO, ps::AbstractArray) where AO <: AndOr =
    ps |> isempty ? AO.instance |> Normal : Normal(AO.instance, map(ps) do p
        Clause(AO.instance |> dual, p)
    end)
# TODO: see `https://en.wikipedia.org/wiki/Tseytin_transformation`
# TODO: fix excessive recompilation dependent on `length(p.clauses)`
Normal(::AO, p::Normal) where AO <: AndOr = Normal(AO.instance,
    map(Iterators.product(map(p.clauses) do clause
        clause.literals
    end...)) do literals
        Clause(AO.instance |> dual, literals |> collect)
    end |> vec
)
Normal(::AO, p::Normal{AO}) where AO <: AndOr = p
Normal(::AO, ps::Proposition...) where AO <: AndOr = Normal(AO.instance, ps |> collect)

# Conversions

foreach([:Atom, :Literal, :Tree, :Clause, :Normal]) do P
    @eval $P(p) = convert($P, p)
end

"""
    convert
"""
convert(::Type{Atom}, p::Literal{typeof(identity)}) = p.atom
convert(::Type{Atom}, p::Tree{typeof(identity), <:Tuple{Atom}}) = p.node |> only
convert(::Type{Literal}, p::Tree{UO, <:Tuple{Atom}}) where UO <: UnaryOperator =
    p.node |> only |> UO.instance |> Literal
convert(::Type{LT}, p::Atom) where LT <: Union{Literal, Tree} = LT(identity, p)
function convert(::Type{Tree}, p::typeof(contradiction))
    p = Atom()
    p ∧ ¬p
end
convert(::Type{Tree}, p::typeof(tautology)) = contradiction |> Tree |> not
convert(::Type{Tree}, p::Literal{UO}) where UO <: UnaryOperator = Tree(UO.instance, p.atom)
convert(::Type{Tree}, p::Clause{AO}) where AO <: AndOr = reduce(AO.instance, p.literals) |> Tree
convert(::Type{Tree}, p::Normal{AO}) where AO <: AndOr = mapreduce(Tree, AO.instance, p.clauses) |> Tree
convert(::Type{Clause}, p::LiteralProposition) = Clause(or, p)
foreach([Clause, Normal]) do CN
    foreach([and, or]) do ao
        @eval convert(::Type{$CN}, p::typeof(identity(:left, $ao))) = $ao |> $CN
    end
end
convert(::Type{Normal}, p::Clause{typeof(and)}) = Normal(or, p)
convert(::Type{Normal}, p::Proposition) = Normal(and, p)

# Bool
Bool(::typeof(tautology)) = true
Bool(::typeof(contradiction)) = false
not(p::Bool) = !p
and(p::Bool, q::Bool) = p && q
or(p::Bool, q::Bool) = p || q
converse_imply(p::Bool, q::Bool) = p ^ q
