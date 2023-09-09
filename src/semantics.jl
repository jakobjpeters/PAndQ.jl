
import Base: ==, convert, Bool, uniontypes
using Base: Fix1, Iterators.product

# Internals

"""
    CallableObjectDocumentation

A dummy type to attach a docstring to a [callable object]
(https://docs.julialang.org/en/v1/manual/methods/#Function-like-objects).

See also this [Documenter.jl issue](https://github.com/JuliaDocs/Documenter.jl/issues/558)
"""
struct CallableObjectDocumentation end

"""
    neutral_operator(::NullaryOperator)

Return a subtype of [`AndOr`](@ref) that is the neutral element of the given [`NullaryOperator`](@ref).

See also [`left_neutrals`](@ref) and [`right_neutrals`](@ref).

# Examples
```jldoctest
julia> PAndQ.neutral_operator(⊤)
and (generic function with 23 methods)

julia> PAndQ.neutral_operator(⊥)
or (generic function with 19 methods)
```
"""
neutral_operator(::typeof(⊤)) = ∧
neutral_operator(::typeof(⊥)) = ∨

"""
    eval_doubles(f, doubles)
"""
eval_doubles(f, doubles) = for double in doubles
    for (left, right) in (double, reverse(double))
        @eval $f(::typeof($left)) = $right
    end
end

# Properties

"""
    dual(::LogicalOperator)

Returns the [`LogicalOperator`](@ref) that is the
[dual](https://en.wikipedia.org/wiki/Boolean_algebra#Duality_principle)
of the given boolean operator.

# Examples
```jldoctest
julia> dual(and)
or (generic function with 19 methods)

julia> @atomize and(p, q) == not(dual(and)(not(p), not(q)))
true

julia> dual(imply)
not_converse_imply (generic function with 6 methods)

julia> @atomize imply(p, q) == not(dual(imply)(not(p), not(q)))
true
```
"""
dual(uo::UnaryOperator) = uo
dual(lo::Union{NullaryOperator, union_typeof((⊻, ↔))}) = ¬lo
eval_doubles(:dual, (
    (∧, ∨),
    (⊼, ⊽),
    (⊻, ↔),
    (→, ↚),
    (↛, ←)
))

"""
    converse(::LogicalOperator)

Returns the [`LogicalOperator`](@ref) that is the
[converse](https://en.wikipedia.org/wiki/Converse_(logic))
of the given boolean operator.

# Examples
```jldoctest
julia> converse(and)
and (generic function with 23 methods)

julia> @atomize and(p, q) == converse(and)(q, p)
true

julia> converse(imply)
converse_imply (generic function with 6 methods)

julia> @atomize imply(p, q) == converse(imply)(q, p)
true
```
"""
converse(co::CommutativeOperator) = co
eval_doubles(:converse, ((→, ←), (↛, ↚)))

"""
    left_neutrals(::LogicalOperator)

Return the corresponding left identity elements of the operator.
The identity elements can be [`tautology`](@ref), [`contradiction`](@ref), neither (empty set), or both.

# Examples
```jldoctest
julia> left_neutrals(or)
Set{Union{typeof(contradiction), typeof(tautology)}} with 1 element:
  PAndQ.contradiction

julia> left_neutrals(imply)
Set{Union{typeof(contradiction), typeof(tautology)}} with 1 element:
  PAndQ.tautology

julia> left_neutrals(nor)
Set{Union{typeof(contradiction), typeof(tautology)}}()
```
"""
left_neutrals(::union_typeof((∧, ↔, →))) = Set{NullaryOperator}((⊤,))
left_neutrals(::union_typeof((∨, ⊻, ↚))) = Set{NullaryOperator}((⊥,))
left_neutrals(::LogicalOperator) = Set{NullaryOperator}()

"""
    right_neutrals(::LogicalOperator)

Return the corresponding right identity elements of the operator.
The identity elements can be [`tautology`](@ref), [`contradiction`](@ref), neither (empty set), or both.

# Examples
```jldoctest
julia> right_neutrals(or)
Set{Union{typeof(contradiction), typeof(tautology)}} with 1 element:
  PAndQ.contradiction

julia> right_neutrals(converse_imply)
Set{Union{typeof(contradiction), typeof(tautology)}} with 1 element:
  PAndQ.tautology
```
"""
right_neutrals(::union_typeof((∧, ↔, ←))) = Set{NullaryOperator}((⊤,))
right_neutrals(::union_typeof((∨, ⊻, ↛))) = Set{NullaryOperator}((⊥,))
right_neutrals(::LogicalOperator) = Set{NullaryOperator}()

# Truths

"""
    valuations(atoms)
    valuations(::Proposition)

Return an iterator of every possible [valuation]
(https://en.wikipedia.org/wiki/Valuation_(logic))
of [`Atom`](@ref)s.

# Examples
```jldoctest
julia> @atomize collect(valuations(p))
2-element Vector{Vector}:
 Pair{Variable, typeof(tautology)}[Variable(:p) => PAndQ.tautology]
 Pair{Variable, typeof(contradiction)}[Variable(:p) => PAndQ.contradiction]

julia> @atomize collect(valuations(p ∧ q))
4-element Vector{Vector}:
 Pair{Variable, typeof(tautology)}[Variable(:p) => PAndQ.tautology, Variable(:q) => PAndQ.tautology]
 Pair{Variable}[Variable(:p) => PAndQ.contradiction, Variable(:q) => PAndQ.tautology]
 Pair{Variable}[Variable(:p) => PAndQ.tautology, Variable(:q) => PAndQ.contradiction]
 Pair{Variable, typeof(contradiction)}[Variable(:p) => PAndQ.contradiction, Variable(:q) => PAndQ.contradiction]
```
"""
function valuations(atoms)
    unique_atoms = unique(atoms)
    n = length(unique_atoms)

    Iterators.map(i -> map(
        (atom, digit) -> atom => Bool(digit) ? ⊥ : ⊤,
        unique_atoms, digits(i, base = 2, pad = n)
    ), 0:BigInt(2) ^ n - 1)
end
valuations(p::Proposition) = valuations(atoms(p))

"""
    interpret(valuation, ::Proposition)

Replaces each [`Atom`](@ref) `p` in the given
[`Proposition`](@ref) with `valuation(p)`, then simplifies.

Calling `p` with an incomplete mapping will partially interpret it.

See also [`tautology`](@ref) and [`contradiction`](@ref).

# Examples
```jldoctest
julia> @atomize interpret(a -> ⊤, ¬p)
contradiction (generic function with 1 method)

julia> @atomize interpret(a -> get(Dict(p => ⊤), a, a), p ∧ q)
q
```
"""
interpret(valuation, p::Atom) = valuation(p)
interpret(valuation, p::Literal{UO}) where UO =
    UO.instance(interpret(valuation, p.atom))
interpret(valuation, p::Tree{LO}) where LO =
    LO.instance(map(node -> interpret(valuation, node), p.nodes)...)
function interpret(valuation, p::Union{Clause{AO}, Normal{AO}}) where AO
    neutral = only(left_neutrals(AO.instance))
    not_neutral = ¬neutral
    q = union_all_type(p)(AO.instance)

    for r in only_field(p)
        s = interpret(valuation, r)
        s == not_neutral && return not_neutral
        q = AO.instance(q, s)
    end

    isempty(only_field(q)) ? neutral : q
end

"""
    (::Proposition)(valuation)
    (::Proposition)(valuation...)

Equivalent to [`interpret(a -> get(Dict(valuation), a, a), p)`](@ref interpret).

See also [`Proposition`](@ref).

# Examples
```jldoctest
julia> @atomize (¬p)(p => ⊤)
contradiction (generic function with 1 method)

julia> @atomize p = Clause(and, [q, r, s])
q ∧ r ∧ s

julia> @atomize p(q => ⊤, r => ⊤)
s
```
"""
function interpret(::CallableObjectDocumentation) end
(p::Proposition)(valuation::Dict) = interpret(a -> get(valuation, a, a), p)
(p::Proposition)(valuation) = p(Dict(valuation))
(p::Proposition)(valuation...) = p(valuation)

"""
    interpretations(p, valuations = valuations(p))

Return an iterator of truth values given by [`interpret`](@ref)ing
`p` with each [`valuation`](@ref valuations).

# Examples
```jldoctest
julia> @atomize collect(interpretations(p))
2-element Vector{Function}:
 tautology (generic function with 1 method)
 contradiction (generic function with 1 method)

julia> @atomize collect(interpretations(p → q, [p => ⊤]))
1-element Vector{Variable}:
 q
```
"""
interpretations(p, valuations = valuations(p)) =
    Iterators.map(valuation -> p(valuation), valuations)

"""
    solve(p)

Return a vector containing all [`interpretations`](@ref) such that
`interpret(p, valuation) == ⊤`.

See also [`interpret`](@ref) and [`tautology`](@ref).

# Examples
```jldoctest
julia> @atomize collect(solve(p))
1-element Vector{Vector{Pair{Variable, typeof(tautology)}}}:
 [Variable(:p) => PAndQ.tautology]

julia> @atomize collect(solve(p ⊻ q))
2-element Vector{Vector{Pair{Variable}}}:
 [Variable(:p) => PAndQ.contradiction, Variable(:q) => PAndQ.tautology]
 [Variable(:p) => PAndQ.tautology, Variable(:q) => PAndQ.contradiction]
```
"""
solve(p) = Iterators.filter(valuation -> p(valuation) == ⊤, valuations(p))

# Predicates

"""
    ==(::Union{NullaryOperator, Proposition}, ::Union{NullaryOperator, Proposition})
    p == q

Returns a boolean indicating whether `p` and `q` are [logically equivalent]
(https://en.wikipedia.org/wiki/Logical_equivalence).

[`Constant`](@ref)s are equivalent if and only if their values are equivalent.

!!! info
    The `≡` symbol is sometimes used to represent logical equivalence.
    However, Julia uses `≡` as an alias for the builtin function `===`
    which cannot have methods added to it.
    Use `==` and `===` to test for equivalence and identity, respectively.

See also [`NullaryOperator`](@ref) and [`Proposition`](@ref).

# Examples
```jldoctest
julia> @atomize p == ¬p
false

julia> @atomize ¬(p ⊻ q) == (p → q) ∧ (p ← q)
true

julia> @atomize ¬(p ⊻ q) === (p → q) ∧ (p ← q)
false
```
"""
==(p::Constant, q::Constant) = p.value == q.value
==(p::Union{NullaryOperator, Proposition}, q::Union{NullaryOperator, Proposition}) =
    is_tautology(p ↔ q)

"""
    is_tautology(p)

Returns a boolean on whether `p` is a [`tautology`](@ref).

# Examples
```jldoctest
julia> is_tautology(⊤)
true

julia> @atomize is_tautology(p)
false

julia> @atomize is_tautology(¬(p ∧ ¬p))
true
```
"""
is_tautology(::typeof(⊤)) = true
is_tautology(::typeof(⊥)) = false
is_tautology(::LiteralProposition) = false
is_tautology(p) = all(isequal(⊤), interpretations(p))

"""
    is_contradiction(p)

Returns a boolean on whether `p` is a [`contradiction`](@ref).

# Examples
```jldoctest
julia> is_contradiction(⊥)
true

julia> @atomize is_contradiction(p)
false

julia> @atomize is_contradiction(p ∧ ¬p)
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

julia> @atomize is_truth(p ∧ ¬p)
true

julia> @atomize is_truth(p)
false

julia> @atomize is_truth(p ∧ q)
false
```
"""
is_truth(::NullaryOperator) = true
is_truth(::LiteralProposition) = false
is_truth(p) = allequal(interpretations(p))

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

julia> @atomize is_contingency(p ∧ ¬p)
false

julia> @atomize is_contingency(p)
true

julia> @atomize is_contingency(p ∧ q)
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

julia> @atomize is_satisfiable(p ∧ ¬p)
false

julia> @atomize is_satisfiable(p)
true

julia> @atomize is_satisfiable(p ∧ q)
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

julia> @atomize is_falsifiable(¬(p ∧ ¬p))
false

julia> @atomize is_falsifiable(p)
true

julia> @atomize is_falsifiable(p ∧ q)
true
```
"""
is_falsifiable(p) = !is_tautology(p)

# Operators

## Generic

⊤() = ⊤
⊥() = ⊥
p ⊼ q = ¬(p ∧ q)
p ⊽ q = ¬p ∧ ¬q
p ∨ q = ¬(p ⊽ q)
p ⊻ q = (p ∨ q) ∧ (p ⊼ q)
p ↔ q = (p → q) ∧ (p ← q)
p ↛ q = p ∧ ¬q
p → q = ¬(p ↛ q)
p ↚ q = ¬p ∧ q
p ← q = ¬(p ↚ q)

## Bool

Bool(::typeof(tautology)) = true
Bool(::typeof(contradiction)) = false
¬p::Bool = !p
p::Bool ∧ q::Bool = p && q
p::Bool ∨ q::Bool = p || q

## Operators

eval_doubles(:not, (
    (⊤, ⊥), (identity, ¬), (∧, ⊼), (∨, ⊽), (⊻, ↔), (→, ↛), (←, ↚)
))

## Propositions

not(p::Atom) = Literal(¬, p)
not(p::Literal{UO}) where UO = (¬UO.instance)(p.atom)
not(p::Tree{LO}) where LO = (¬LO.instance)(p.nodes...)
not(p::Union{Clause{AO}, Normal{AO}}) where AO <: AndOr =
    union_all_type(p)(dual(AO.instance), map(¬, only_field(p)))

::typeof(⊤) ∧ ::typeof(⊤) = ⊤
::typeof(⊥) ∧ q::Union{NullaryOperator, Proposition} = ⊥ # domination law
::typeof(⊤) ∧ q::Union{NullaryOperator, Proposition} = q # identity law
p::Proposition ∧ q::NullaryOperator = q ∧ p # commutative property
p::Proposition ∧ q::Proposition = Normal(∧, p) ∧ Normal(∧, q)

for BO in uniontypes(BinaryOperator)
    bo = nameof(BO.instance)
    @eval $bo(p) = Fix1($bo, p)
    @eval $bo(p::Tree, q::Tree) = Tree($bo, p, q)
    @eval $bo(p::Tree, q::Union{Atom, Literal}) = Tree($bo, p, Tree(q))
    @eval $bo(p::Union{Atom, Literal}, q::Tree) = Tree($bo, Tree(p), q)
    @eval $bo(p::Union{Atom, Literal}, q::Union{Atom, Literal}) =
        $bo(Tree(p), Tree(q))
end

for AO in uniontypes(AndOr)
    ao = nameof(AO.instance)
    dao = nameof(dual(AO.instance))
    DAO = typeof(dual(AO.instance))

    @eval $ao(p::Clause{$DAO}, q::Clause{$DAO}) = Normal($ao, [p, q])

    @eval $ao(p::Union{LiteralProposition, Clause{$AO}}, q::Clause{$DAO}) =
        $ao(Normal($ao, p), q)
    @eval $ao(p::Clause{$DAO}, q::Union{LiteralProposition, Clause{$AO}}) =
        $ao(p, Normal($ao, q))

    for (left, right) in ((Clause, LiteralProposition), (Normal, Clause{DAO}))
        @eval $ao(p::$left{$AO}, q::$right) = $left($ao, vcat(only_field(p), q))
        @eval $ao(p::$right, q::$left{$AO}) = $left($ao, vcat(p, only_field(q)))
    end

    for CN in (Clause, Normal)
        @eval $ao(p::$CN{$AO}, q::$CN{$AO}) =
            $CN($ao, vcat(only_field(p), only_field(q)))
    end

    @eval $ao(p::Normal, q::Normal) = $ao(Normal($ao, p), Normal($ao, q))
    @eval $ao(p::Clause, q::Normal) = $ao(Normal($ao, p), q)
    @eval $ao(p::Normal, q::Clause) = $ao(p, Normal($ao, q))
end

# Constructors

Clause(ao::AndOr, ps) = isempty(ps) ?
    Clause(ao) :
    Clause(ao, collect(map(Literal, ps)))
Clause(::AO, p::Proposition) where AO <: AndOr = convert(Clause{AO}, p)

Normal(ao::AndOr, ps) = isempty(ps) ?
    Normal(ao) :
    Normal(ao, collect(map(p -> Clause(dual(ao), [p]), ps)))
Normal(::AO, p::Proposition) where AO <: AndOr = convert(Normal{AO}, p)

for P in (:Literal, :Tree, :Clause, :Normal)
    @eval $P(p) = convert($P, p)
end

# Utilities

"""
    convert(::Type{<:Proposition}, ::Union{NullaryOperator, Proposition})
"""
convert(::Type{Atom}, p::Literal{typeof(identity)}) = p.atom
convert(::Type{Atom}, p::Tree{typeof(identity), <:Atom}) = only(p.nodes)
convert(::Type{Literal}, p::Tree{UO, <:Atom}) where UO =
    Literal(UO.instance(only(p.nodes)))
convert(::Type{LT}, p::Atom) where LT <: Union{Literal, Tree} = LT(identity, p)
convert(::Type{Tree}, p::Literal{UO}) where UO = Tree(UO.instance, p.atom)
convert(::Type{Tree}, p::Clause{AO}) where AO = Tree(foldl(AO.instance, p.literals))
convert(::Type{Tree}, p::Normal{AO}) where AO = Tree(mapfoldl(Tree, AO.instance, p.clauses))
convert(::Type{Clause}, p::LiteralProposition) = Clause(or, [p])
convert(::Type{Clause{AO}}, p::LiteralProposition) where AO <: AndOr = Clause(AO.instance, [p])
convert(::Type{Clause}, no::NullaryOperator) = Clause(neutral_operator(no))
convert(::Type{Normal}, no::NullaryOperator) = Normal(neutral_operator(no))
convert(::Type{Clause}, p::Tree{NO}) where NO <: NullaryOperator = Clause(NO.instance)
convert(::Type{Normal}, p::Tree{NO}) where NO <: NullaryOperator = Normal(NO.instance)
convert(::Type{Normal}, p::Clause{AO}) where AO <: AndOr = Normal(dual(AO.instance), [p])
convert(::Type{Normal{AO}}, p::LiteralProposition) where AO <: AndOr = Normal(AO.instance, Clause(p))
convert(::Type{Normal{AO}}, p::Clause{AO}) where AO <: AndOr =
    Normal(AO.instance, map(literal -> Clause(dual(AO.instance), literal), p.literals))
convert(::Type{Normal{AO}}, p::Clause) where AO <: AndOr = Normal(AO.instance, [p])
convert(::Type{Normal}, p::Proposition) = Normal(∧, p)
convert(::Type{Normal{AO}}, p::Tree{LO}) where {AO, LO} = Normal(AO.instance, LO.instance(
    map(node -> Normal(node), p.nodes)...
))
convert(::Type{Normal{AO}}, p::Normal{AO}) where AO <: AndOr = p
convert(::Type{Normal{AO}}, p::Normal) where AO <: AndOr = Normal(AO.instance,
    vec(map(product(map(p.clauses) do clause
        clause.literals
    end...)) do literals
        Clause(dual(AO.instance), literals)
    end)
)
