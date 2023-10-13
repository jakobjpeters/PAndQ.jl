
import Base: ==, convert, Bool
using Base: Iterators.product, uniontypes

# Internals

"""
    neutral_operator(::NullaryOperator)

Return a subtype of [`AndOr`](@ref) that is the neutral element of the given [`NullaryOperator`](@ref).

See also [`left_neutrals`](@ref) and [`right_neutrals`](@ref).

# Examples
```jldoctest
julia> PAndQ.neutral_operator(⊤)
& (generic function with 34 methods)

julia> PAndQ.neutral_operator(⊥)
| (generic function with 34 methods)
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
| (generic function with 34 methods)

julia> @atomize p ∧ q == ¬dual(∧)(¬p, ¬q)
true

julia> dual(→)
not_converse_imply (generic function with 3 methods)

julia> @atomize (p → q) == ¬dual(→)(¬p, ¬q)
true
```
"""
dual(uo::UnaryOperator) = uo
dual(no::NullaryOperator) = ¬no
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
julia> converse(∧)
& (generic function with 34 methods)

julia> @atomize p ∧ q == converse(∧)(q, p)
true

julia> converse(→)
converse_imply (generic function with 3 methods)

julia> @atomize (p → q) == converse(→)(q, p)
true
```
"""
converse(co::CommutativeOperator) = co
eval_doubles(:converse, ((→, ←), (↛, ↚)))

"""
    left_neutrals(::LogicalOperator)

Return the corresponding left identity elements of the operator.
The identity elements can be neither, either, or both of
[`tautology`](@ref) and [`contradiction`](@ref).

# Examples
```jldoctest
julia> left_neutrals(∨)
Set{typeof(contradiction)} with 1 element:
  PAndQ.contradiction

julia> left_neutrals(→)
Set{typeof(tautology)} with 1 element:
  PAndQ.tautology

julia> left_neutrals(⊽)
Set{Union{typeof(contradiction), typeof(tautology)}}()
```
"""
left_neutrals(::union_typeof((∧, ↔, →))) = Set((⊤,))
left_neutrals(::union_typeof((∨, ⊻, ↚))) = Set((⊥,))
left_neutrals(::LogicalOperator) = Set{NullaryOperator}()

"""
    right_neutrals(::LogicalOperator)

Return the corresponding right identity elements of the operator.
The identity elements can be neither, either, or both of
[`tautology`](@ref) and [`contradiction`](@ref).

# Examples
```jldoctest
julia> right_neutrals(∨)
Set{typeof(contradiction)} with 1 element:
  PAndQ.contradiction

julia> right_neutrals(←)
Set{typeof(tautology)} with 1 element:
  PAndQ.tautology

julia> left_neutrals(⊽)
Set{Union{typeof(contradiction), typeof(tautology)}}()
```
"""
right_neutrals(::union_typeof((∧, ↔, ←))) = Set((⊤,))
right_neutrals(::union_typeof((∨, ⊻, ↛))) = Set((⊥,))
right_neutrals(::LogicalOperator) = Set{NullaryOperator}()

# Truths

"""
    valuations(atoms)
    valuations(::Proposition)

Return an iterator of every possible [valuation]
(https://en.wikipedia.org/wiki/Valuation_(logic))
of [`Atom`](@ref)s.

See also [`Proposition`](@ref).

# Examples
```jldoctest
julia> @atomize collect(valuations(p))
2-element Vector{Vector{Pair{PAndQ.Variable, Bool}}}:
 [PAndQ.Variable(:p) => 1]
 [PAndQ.Variable(:p) => 0]

julia> @atomize collect(valuations(p ∧ q))
2×2 Matrix{Vector{Pair{PAndQ.Variable, Bool}}}:
 [Variable(:p)=>1, Variable(:q)=>1]  [Variable(:p)=>1, Variable(:q)=>0]
 [Variable(:p)=>0, Variable(:q)=>1]  [Variable(:p)=>0, Variable(:q)=>0]
```
"""
function valuations(atoms)
    unique_atoms = unique(atoms)
    n = length(unique_atoms)

    Iterators.map(
        truths -> map(Pair, unique_atoms, truths),
        Iterators.product(repeat([[true, false]], n)...)
    )
end
valuations(p::Proposition) = valuations(atoms(p))

"""
    interpret(valuation, ::Proposition)

Substitute each [`Atom`](@ref) in the given
[`Proposition`](@ref) with values from the `valuation`.

The `valuation` can be a [function]
(https://docs.julialang.org/en/v1/manual/functions/)
or [function-like object]
(https://docs.julialang.org/en/v1/manual/methods/#Function-like-objects)
with the signature `valuation(::Atom)::Union{Bool, NullaryOperator}`,
a [`Dict`](https://docs.julialang.org/en/v1/base/collections/#Base.Dict),
or an iterable that can construct a `Dict`.
No substitution is performed if an [`Atom`](@ref) from the
[`Proposition`](@ref) is not one of the dictionary's keys.

See also [`NullaryOperator`](@ref).

# Examples
```jldoctest
julia> @atomize interpret(p -> true, ¬p)
false

julia> @atomize interpret(p => true, p ∧ q)
q
```
"""
interpret(valuation::Function, p::Atom) = valuation(p)
interpret(valuation::Function, p::Union{Literal, Tree}) =
    nodevalue(p)(map(child -> interpret(valuation, child), children(p))...)
interpret(valuation::Function, p::Tree{<:NullaryOperator}) = nodevalue(p)()
function interpret(valuation::Function, p::Union{Clause, Normal})
    _nodevalue = nodevalue(p)
    neutral = only(left_neutrals(_nodevalue))()
    not_neutral = ¬neutral
    q = union_all_type(p)(_nodevalue)

    for r in children(p)
        s = interpret(valuation, r)
        s == not_neutral && return not_neutral
        q = _nodevalue(q, s)
    end

    isempty(children(q)) ? neutral : q
end
interpret(valuation::DataType, p) = interpret(a -> valuation(a), p)
interpret(valuation::Dict, p) = interpret(a -> get(valuation, a, a), p)
interpret(valuation, p) = interpret(Dict(valuation), p)

"""
    (::Proposition)(valuation...)

Equivalent to [`interpret(valuation, p)`](@ref interpret).

# Examples
```jldoctest
julia> @atomize ¬p(p => true)
false

julia> @atomize (p ∧ q)(p => true)
q
```
"""
(p::Proposition)(valuation) = interpret(valuation, p)
(p::Proposition)(valuation...) = p(valuation)

"""
    interpretations(p, valuations = valuations(p))

Return an iterator of truth values given by [`interpret`](@ref)ing
`p` with each [`valuation`](@ref valuations).

# Examples
```jldoctest
julia> @atomize collect(interpretations(p))
2-element Vector{Bool}:
 1
 0

julia> @atomize collect(interpretations(p ⊻ q, [p => true]))
1-element Vector{PAndQ.Literal{typeof(!), PAndQ.Variable}}:
 ¬q
```
"""
interpretations(p, valuations = valuations(p)) =
    Iterators.map(valuation -> interpret(Dict(valuation), p), valuations)

"""
    solve(p)

Return a vector containing all [`valuations`](@ref) such that
`interpret(p, valuation) == ⊤`.

See also [`interpret`](@ref) and [`tautology`](@ref).

# Examples
```jldoctest
julia> @atomize collect(solve(p))
1-element Vector{Vector{Pair{PAndQ.Variable, Bool}}}:
 [PAndQ.Variable(:p) => 1]

julia> @atomize collect(solve(p ⊻ q))
2-element Vector{Vector{Pair{PAndQ.Variable, Bool}}}:
 [PAndQ.Variable(:p) => 0, PAndQ.Variable(:q) => 1]
 [PAndQ.Variable(:p) => 1, PAndQ.Variable(:q) => 0]
```
"""
solve(p) = Iterators.filter(valuation -> interpret(valuation, p), valuations(p))

# Predicates

"""
    ==(::Union{Bool, NullaryOperator, Proposition}, ::Union{Bool, NullaryOperator, Proposition})
    p == q

Returns a boolean indicating whether `p` and `q` are [logically equivalent]
(https://en.wikipedia.org/wiki/Logical_equivalence).

[`Constant`](@ref)s are equivalent if and only if their values are equivalent.

!!! info
    The `≡` symbol is sometimes used to represent logical equivalence.
    However, Julia uses `≡` as an alias for the builtin function `===`
    which cannot have methods added to it.

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
p::Constant == q::Constant = p.value == q.value
p::Bool == q::NullaryOperator = p == q()
p::Bool == q::Proposition = p ? is_tautology(q) : is_contradiction(q)
p::Union{NullaryOperator, Proposition} == q::Bool = q == p
::typeof(⊤) == q::Proposition = is_tautology(q)
::typeof(⊥) == q::Proposition = is_contradiction(q)
p::Proposition == q::Proposition = is_tautology(p ↔ q)
p::Proposition == q::NullaryOperator = q == p

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
is_tautology(::Union{typeof(⊥), Atom, Literal}) = false
is_tautology(p) = all(isequal(true), interpretations(p))

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
is_truth(::Union{Atom, Literal}) = false
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

julia> @atomize is_falsifiable(p ∨ ¬p)
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

⊤() = true
⊥() = false

p::Bool ∧ q::Union{NullaryOperator, Proposition} = p && q
p::Union{NullaryOperator, Proposition} ∧ q::Bool = q ∧ p

for (left, right) in (
    :⊼ => :(¬(p ∧ q)),
    :⊽ => :(¬p ∧ ¬q),
    :∨ => :(¬(p ⊽ q)),
    :⊻ => :((p ∨ q) ∧ (p ⊼ q)),
)
    @eval $left(p::Union{NullaryOperator, Proposition}, q::Bool) = $left(q, p)
    @eval $left(p::Union{Bool, NullaryOperator, Proposition}, q::Union{NullaryOperator, Proposition}) =
        $right
end

for (left, right) in (
    :↔ => :((p ∧ q) ∨ (p ⊽ q)),
    :↛ => :(p ∧ ¬q),
    :→ => :(¬p ∨ q),
    :↚ => :(¬p ∧ q),
    :← => :(p ∨ ¬q)
)
    @eval $left(p::Union{Bool, NullaryOperator, Proposition}, q::Union{Bool, NullaryOperator, Proposition}) =
        $right
end

## Bool

Bool(no::NullaryOperator) = convert(Bool, no)

## Operators

eval_doubles(:not, (
    (⊤, ⊥), (∧, ⊼), (∨, ⊽), (⊻, ↔), (→, ↛), (←, ↚)
))

## Propositions

¬p::Atom = Literal(¬, p)
(¬p::Union{Literal, Tree}) = Tree(¬, Tree(p))
(¬p::Clause) = Clause(dual(nodevalue(p)), map(
    _child -> Literal(¬nodevalue(_child), child(_child)),
children(p)))
(¬p::Normal) = Normal(dual(nodevalue(p)), map(¬, children(p)))

p::Proposition ∧ q::Proposition = Normal(∧, p) ∧ Normal(∧, q)

for BO in uniontypes(BinaryOperator)
    bo = nameof(BO.instance)
    @eval $bo(p::Union{NullaryOperator, Proposition}) = q -> $bo(p, q)
    @eval $bo(p::Union{NullaryOperator, Atom, Literal, Tree}, q::Union{NullaryOperator, Atom, Literal, Tree}) =
        Tree($bo, Tree(p), Tree(q))
end

for AO in uniontypes(AndOr)
    ao = nameof(AO.instance)
    dao = nameof(dual(AO.instance))
    DAO = typeof(dual(AO.instance))

    @eval begin
        $ao(p::Clause{$DAO}, q::Clause{$DAO}) = Normal($ao, [p, q])

        $ao(p::Union{Atom, Literal, Clause{$AO}}, q::Clause{$DAO}) =
            $ao(Normal($ao, p), q)
        $ao(p::Clause{$DAO}, q::Union{Atom, Literal, Clause{$AO}}) =
            $ao(p, Normal($ao, q))

        $ao(p::Normal, q::Normal) = $ao(Normal($ao, p), Normal($ao, q))
        $ao(p::Clause, q::Normal) = $ao(Normal($ao, p), q)
        $ao(p::Normal, q::Clause) = $ao(p, Normal($ao, q))
    end

    for (left, right) in ((Clause, Union{Atom, Literal}), (Normal, Clause{DAO}))
        @eval begin
            $ao(p::$left{$AO}, q::$right) = $left($ao, vcat(children(p), q))
            $ao(p::$right, q::$left{$AO}) = $left($ao, vcat(p, children(q)))
            $ao(p::$left{$AO}, q::$left{$AO}) =
                $left($ao, vcat(children(p), children(q)))
        end
    end
end

# Constructors

for T in (:Literal, :Tree)
    @eval $T(::ComposedFunction{typeof(¬), typeof(𝒾)}, p) = $T(¬, p)
    @eval $T(::ComposedFunction{typeof(¬), typeof(¬)}, p) = $T(𝒾, p)
end

Clause(ao::AndOr, ps) = isempty(ps) ?
    Clause(ao) :
    Clause(ao, collect(map(Literal, ps)))
Clause(::AO, p::Proposition) where AO <: AndOr = convert(Clause{AO}, p)

Normal(ao::AndOr, ps) = isempty(ps) ?
    Normal(ao) :
    Normal(ao, collect(map(p -> Clause(dual(ao), [p]), ps)))
Normal(::AO, p::Proposition) where AO <: AndOr = convert(Normal{AO}, p)

for P in (:Atom, :Literal, :Tree, :Clause, :Normal)
    @eval $P(p) = convert($P, p)
end

# Utilities

"""
    convert(::Type{<:Proposition}, ::Union{NullaryOperator, Proposition})
"""
convert(::Type{Bool}, no::NullaryOperator) = no()
convert(::Type{Atom}, p::Union{Literal{I}, Tree{I, <:Atom}}) where I <: typeof(𝒾) =
    child(p)
convert(::Type{Literal}, p::Tree{<:UnaryOperator, <:Atom}) =
    Literal(nodevalue(p), child(p))
convert(::Type{Literal}, p::Atom) = Literal(𝒾, p)
convert(::Type{Tree}, p::Atom) = Tree(𝒾, p)
convert(::Type{Tree}, p::Literal) = Tree(nodevalue(p), p.atom)
function convert(::Type{Tree}, p::Clause)
    _nodevalue = nodevalue(p)
    Tree(foldl(_nodevalue, p.literals; init = only(left_neutrals(_nodevalue))))
end
function convert(::Type{Tree}, p::Normal)
    _nodevalue = nodevalue(p)
    Tree(mapfoldl(Tree, _nodevalue, p.clauses; init = only(left_neutrals(_nodevalue))))
end
convert(::Type{Clause}, p::Union{Atom, Literal}) = Clause(or, [p])
convert(::Type{Clause{AO}}, p::Union{Atom, Literal}) where AO <: AndOr = Clause(AO.instance, [p])
convert(::Type{Clause}, no::NullaryOperator) = Clause(neutral_operator(no))
convert(::Type{Normal}, no::NullaryOperator) = Normal(neutral_operator(no))
convert(::Type{Clause}, p::Tree{<:NullaryOperator}) = Clause(nodevalue(p))
convert(::Type{Normal}, p::Tree{<:NullaryOperator}) = Normal(nodevalue(p))
convert(::Type{Normal}, p::Clause) = Normal(dual(nodevalue(p)), [p])
convert(::Type{Normal{AO}}, p::Union{Atom, Literal}) where AO <: AndOr = Normal(AO.instance, Clause(p))
convert(::Type{Normal{AO}}, p::Clause{AO}) where AO <: AndOr =
    Normal(AO.instance, map(literal -> Clause(dual(AO.instance), literal), p.literals))
convert(::Type{Normal{AO}}, p::Clause) where AO <: AndOr = Normal(AO.instance, [p])
convert(::Type{Normal}, p::Proposition) = Normal(∧, p)
convert(::Type{Normal{AO}}, p::Tree) where AO =
    Normal(AO.instance, nodevalue(p)(map(Normal, p.nodes)...))
convert(::Type{Normal{AO}}, p::Normal{AO}) where AO <: AndOr = p
convert(::Type{Normal{AO}}, p::Normal) where AO <: AndOr = Normal(AO.instance,
    vec(map(product(map(p.clauses) do clause
        clause.literals
    end...)) do literals
        Clause(dual(AO.instance), literals)
    end)
)
