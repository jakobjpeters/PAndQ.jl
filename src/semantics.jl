
import Base: ==, convert, Bool

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
    UO.instance(interpret(p.atom, valuation))
function interpret(p::CN, valuation::Dict) where {AO <: AndOr, CN <: Union{Clause{AO}, Normal{AO}}}
    neutral_element = identity(:left, AO.instance)
    not_neutral_element = not(neutral_element)
    q = getfield(Main, nameof(CN))(AO.instance)
    for r in getfield(p, 1)
        s = interpret(r, valuation)
        s == not_neutral_element && return not_neutral_element
        q = AO.instance(q, s)
    end
    return isempty(getfield(q, 1)) ? neutral_element : q
end
interpret(p::Proposition, valuation::Dict) = interpret(Normal(and, p), valuation)
interpret(p, valuation) = interpret(p, Dict(valuation))
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
    n = length(atoms)
    return map(0:2 ^ n - 1) do i
        map(zip(atoms, digits(i, base = 2, pad = n))) do (left, right)
            left => right == 0 ? ⊤ : ⊥
        end
    end
end
valuations(p::Proposition) = valuations(atoms(p))
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
    solve(p)

Return a vector containing all [`interpretations`](@ref) such that
`interpret(p, interpretation) == ⊤`.

# Examples
```jldoctest
julia> @p solve(p)
1-element Vector{Vector{Pair{Atom{Symbol}, typeof(tautology)}}}:
 [p => ⊤]

julia> @p solve(p ⊻ q)
2-element Vector{Vector{Pair{Atom{Symbol}}}}:
 [p => ⊥, q => ⊤]
 [p => ⊤, q => ⊥]
```
"""
function solve(p)
    _valuations = valuations(p)
    _interpretations = interpretations(p, _valuations)
    return map(filter(collect(zip(_valuations, _interpretations))) do (valuation, interpretation)
        interpretation == ⊤
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
identity(x, binary_operator::BinaryOperator) = identity(Val(x), binary_operator)
foreach([(:and, :xnor, :⊤), (:or, :xor, :⊥)]) do (left, middle, right)
    @eval identity(::Union{Val{:left}, Val{:right}}, ::Union{map(typeof, [$left, $middle])...}) = $right
end
foreach([
    (:left, :imply, :⊤),
    (:right, :not_imply, :⊥),
    (:right, :converse_imply, :⊤),
    (:left, :not_converse_imply, :⊥)
]) do (left, middle, right)
    @eval identity(::$(typeof(Val(left))), ::typeof($middle)) = $right
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
foreach([(imply, converse_imply), (not_imply, not_converse_imply)]) do double
    foreach([double, reverse(double)]) do (left, right)
        @eval converse(::typeof($left)) = $right
    end
end

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
dual(::BO) where BO <: Union{
    map(typeof, [tautology, contradiction, xor, xnor])...
} = not(BO.instance)
foreach([
    (and, or),
    (nand, nor),
    (xor, xnor),
    (imply, not_converse_imply),
    (not_imply, converse_imply)
]) do double
    foreach([double, reverse(double)]) do (left, right)
        @eval dual(::typeof($left)) = $right
    end
end
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
is_tautology(p) = all(Base.Fix1(==, ⊤), interpretations(p))
is_tautology(p::CN) where {A <: typeof(and), CN <: Union{Clause{A}, Normal{A}}} =
    isempty(getfield(p, 1))

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
is_truth(p) = length(unique(interpretations(p))) == 1

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
is_contingency(p) = !is_truth(p)

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
is_satisfiable(p) = !is_contradiction(p)

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
is_falsifiable(p) = !is_tautology(p)

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
foreach([:and, :or]) do and_or
    @eval $(Symbol(:reduce_, and_or))(xs) = reduce($and_or, xs)
end

# boolean operators
foreach([
    (tautology, contradiction),
    (identity, not),
    (and, nand),
    (or, nor),
    (xor, xnor),
    (imply, not_imply),
    (converse_imply, not_converse_imply)
]) do double
    foreach([double, reverse(double)]) do (left, right)
        @eval not(::typeof($left)) = $right
    end
end

# propositions
not(p::Atom) = Literal(not, p)
not(p::Literal{UO}) where UO <: UnaryOperator = not(UO.instance)(p.atom)
not(p::Tree{BO}) where BO <: BooleanOperator = not(BO.instance)(p.node...)
not(p::CN) where CN <: Union{Clause{AO}, Normal{AO}} where AO <: AndOr =
    getfield(Main, nameof(CN))(dual(AO.instance), map(not, getfield(p, 1)))

and(::typeof(tautology), ::typeof(tautology)) = ⊤
and(::typeof(contradiction), ::Union{NullaryOperator, Proposition}) = ⊥ # domination law
and(::typeof(tautology), q::Union{NullaryOperator, Proposition}) = q # identity law
and(p::Proposition, q::NullaryOperator) = q ∧ p # commutative property
and(p::Proposition, q::Proposition) = and(Normal(and, p), Normal(and, q))

foreach(Base.uniontypes(BinaryOperator)) do BO
    binary_operator = Symbol(BO.instance)
    @eval $binary_operator(p) = Base.Fix1($binary_operator, p)
    @eval $binary_operator(p::Tree, q::Tree) = Tree($binary_operator, p, q)
    @eval $binary_operator(p::Tree, q::Proposition) = Tree($binary_operator, p, Tree(q))
    @eval $binary_operator(p::Proposition, q::Tree) = Tree($binary_operator, Tree(p), q)
    @eval $binary_operator(p::Union{Atom, Literal}, q::Union{Atom, Literal}) = $binary_operator(Tree(p), Tree(q))
end

foreach(Base.uniontypes(AndOr)) do AndOr
    and_or = Symbol(AndOr.instance)
    dual_and_or = Symbol(dual(AndOr.instance))
    DualAndOr = typeof(dual(AndOr.instance))

    @eval $and_or(p::Clause{$DualAndOr}, q::Clause{$DualAndOr}) = Normal($and_or, p, q)

    @eval $and_or(p::Union{LiteralProposition, Clause{$AndOr}}, q::Clause{$DualAndOr}) =
        $and_or(Normal($and_or, p), q)
    @eval $and_or(p::Clause{$DualAndOr}, q::Union{LiteralProposition, Clause{$AndOr}}) =
        $and_or(p, Normal($and_or, q))

    foreach([(Clause, LiteralProposition), (Normal, Clause{DualAndOr})]) do (left, right)
        @eval $and_or(p::$left{$AndOr}, q::$right) = $left($and_or, vcat(getfield(p, 1), q))
        @eval $and_or(p::$right, q::$left{$AndOr}) = $left($and_or, vcat(p, getfield(q, 1)))
    end

    foreach([Clause, Normal]) do ClauseNormal
        @eval $and_or(p::$ClauseNormal{$AndOr}, q::$ClauseNormal{$AndOr}) =
            $ClauseNormal($and_or, vcat(getfield(p, 1), getfield(q, 1)))
    end

    @eval $and_or(p::Normal, q::Normal) = $and_or(Normal($and_or, p), Normal($and_or, q))
    @eval $and_or(p::Clause, q::Normal) = $and_or(Normal($and_or, p), q)
    @eval $and_or(p::Normal, q::Clause) = $and_or(p, Normal($and_or, q))
end

# Constructors

Clause(::AO, ps::AbstractArray) where AO <: AndOr =
    isempty(ps) ? Clause(AO.instance) : Clause(AO.instance, map(Literal, ps))
Clause(::AO, ps...) where AO <: AndOr = Clause(AO.instance, collect(ps))

Normal(::AO, p::Tree{BO}) where {AO <: AndOr, BO <: BooleanOperator} = BO.instance(
    map(p.node) do branch
        Normal(AO.instance, branch)
    end...
)
Normal(::AO, p::Clause{AO}) where AO <: AndOr = Normal(AO.instance, map(p.literals) do literal
    Clause(dual(AO.instance), literal)
end)
Normal(::AO, ps::AbstractArray) where AO <: AndOr =
    isempty(ps) ? Normal(AO.instance) : Normal(AO.instance, map(ps) do p
        Clause(dual(AO.instance), p)
    end)
Normal(::AO, p::Normal) where AO <: AndOr = Normal(AO.instance,
    vec(map(Iterators.product(map(p.clauses) do clause
        clause.literals
    end...)) do literals
        Clause(dual(AO.instance), collect(literals))
    end)
)
Normal(::AO, p::Normal{AO}) where AO <: AndOr = p
Normal(::AO, ps::Proposition...) where AO <: AndOr = Normal(AO.instance, collect(ps))

# Conversions

foreach([:Atom, :Literal, :Tree, :Clause, :Normal]) do P
    @eval $P(p) = convert($P, p)
end

"""
    convert
"""
convert(::Type{Atom}, p::Literal{typeof(identity)}) = p.atom
convert(::Type{Atom}, p::Tree{typeof(identity), <:Tuple{Atom}}) = only(p.node)
convert(::Type{Literal}, p::Tree{UO, <:Tuple{Atom}}) where UO <: UnaryOperator =
    Literal(UO.instance(only(p.node)))
convert(::Type{LT}, p::Atom) where LT <: Union{Literal, Tree} = LT(identity, p)
function convert(::Type{Tree}, p::typeof(contradiction))
    p = Atom()
    return p ∧ ¬p
end
convert(::Type{Tree}, p::typeof(tautology)) = not(Tree(contradiction))
convert(::Type{Tree}, p::Literal{UO}) where UO <: UnaryOperator = Tree(UO.instance, p.atom)
convert(::Type{Tree}, p::Clause{AO}) where AO <: AndOr = Tree(reduce(AO.instance, p.literals))
convert(::Type{Tree}, p::Normal{AO}) where AO <: AndOr = Tree(mapreduce(Tree, AO.instance, p.clauses))
convert(::Type{Clause}, p::LiteralProposition) = Clause(or, p)
foreach([Clause, Normal]) do ClauseNormal
    foreach([and, or]) do and_or
        @eval convert(::Type{$ClauseNormal}, p::typeof(identity(:left, $and_or))) = $ClauseNormal($and_or)
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
