
import Base: ==, convert, Bool, Fix2
using Base: Iterators.product, uniontypes

# Internals

"""
    process_valuations(valuations, p, f)
"""
process_valuations(valuations, p, f) =
    f(valuation -> interpret(valuation, p), valuations)
process_valuations(p, f) =
    f(valuation -> _interpret(p, a -> Dict(valuation)[a], Bool), valuations(p))

"""
    neutral_operator(::NullaryOperator)

Return a subtype of [`AndOr`](@ref) that is the neutral
element of the given [`NullaryOperator`](@ref).

# Examples
```jldoctest
julia> PAndQ.neutral_operator(⊤)
and (generic function with 17 methods)

julia> PAndQ.neutral_operator(⊥)
or (generic function with 17 methods)
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

# Truths

"""
    valuation(atoms)
    valuations(::Union{NullaryOperator, Proposition})

Return an iterator of every possible [valuation]
(https://en.wikipedia.org/wiki/Valuation_(logic)).

See also [Nullary Operators](@ref nullary_operators) and [`Proposition`](@ref).

# Examples
```jldoctest
julia> collect(valuations(⊤))
0-dimensional Array{Vector{Union{}}, 0}:
[]

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
    Iterators.map(valuation -> map(Pair, unique_atoms, valuation),
        Iterators.product(Iterators.repeated([true, false], length(unique_atoms))...)
    )
end
    valuations(p::Union{NullaryOperator, Proposition}) = valuations(collect(atoms(p)))

_interpret(no::NullaryOperator, valuation, convert) = convert(no)
_interpret(p::Atom, valuation, convert) = valuation(p)
_interpret(p::Tree, valuation, convert) =
    nodevalue(p)(map(child -> _interpret(child, valuation, convert), children(p))...)
_interpret(p::Tree{<:NullaryOperator}, valuation, convert) = convert(nodevalue(p))
function _interpret(p::Union{Clause, Normal}, valuation, convert)
    _nodevalue = nodevalue(p)
    neutral = only(left_neutrals(_nodevalue))()
    not_neutral = ¬neutral
    q = union_all_type(p)(_nodevalue)

    for r in children(p)
        s = _interpret(r, valuation, convert)
        s == not_neutral && return not_neutral
        q = _nodevalue(q, s)
    end

    isempty(children(q)) ? neutral : q
end

"""
    interpret(valuation, ::Union{NullaryOperator, Proposition})

Substitute each [`Atom`](@ref) in the given
[`Proposition`](@ref) with values from the `valuation`.

The `valuation` can be a [function]
(https://docs.julialang.org/en/v1/manual/functions/) with the signature
`valuation(::Atom)::Union{Bool, NullaryOperator, Proposition}`, a
[`Dict`](https://docs.julialang.org/en/v1/base/collections/#Base.Dict),
or an iterable that can construct a `Dict`.
No substitution is performed if an [`Atom`](@ref) from the
[`Proposition`](@ref) is not one of the dictionary's keys.

See also [Nullary Operators](@ref nullary_operators).

# Examples
```jldoctest
julia> @atomize interpret(p -> true, ¬p)
false

julia> @atomize interpret(p => ⊤, p ∧ q)
⊤ ∧ q
```
"""
interpret(valuation::Function, p) = _interpret(p, valuation, 𝒾)
interpret(valuation::Dict, p) = interpret(a -> get(valuation, a, a), p)
interpret(valuation, p) = interpret(Dict(valuation), p)

"""
    (::Proposition)(valuation)

Equivalent to [`interpret(valuation, p)`](@ref interpret).

# Examples
```jldoctest
julia> @atomize ¬p([p => ⊤])
¬⊤

julia> @atomize (p ∧ q)([p => ⊤])
⊤ ∧ q
```
"""
(p::Proposition)(valuation) = interpret(valuation, p)

"""
    interpretations(valuations, p)
    interpretations(p)

Return an iterator of truth values given by [`interpret`](@ref)ing
`p` with each valuation in [`valuations`](@ref).

# Examples
```jldoctest
julia> collect(interpretations(⊤))
0-dimensional Array{Bool, 0}:
1

julia> @atomize collect(interpretations(p))
2-element Vector{Bool}:
 1
 0

julia> @atomize collect(interpretations(p ∧ q))
2×2 Matrix{Bool}:
 1  0
 0  0
```
"""
interpretations(valuations, p) = process_valuations(valuations, p, Iterators.map)
interpretations(p) = process_valuations(p, Iterators.map)

"""
    solve(valuations, p)
    solve(p)

Return a vector containing all [`valuations`](@ref) such that
`interpret(valuation, p) == ⊤`.

See also [`interpret`](@ref) and [`tautology`](@ref).

# Examples
```jldoctest
julia> collect(solve(⊤))
1-element Vector{Vector{Union{}}}:
 []

julia> @atomize collect(solve(p))
1-element Vector{Vector{Pair{PAndQ.Variable, Bool}}}:
 [PAndQ.Variable(:p) => 1]

julia> @atomize collect(solve(p ∧ q))
1-element Vector{Vector{Pair{PAndQ.Variable, Bool}}}:
 [PAndQ.Variable(:p) => 1, PAndQ.Variable(:q) => 1]
```
"""
solve(valuations, p) = process_valuations(valuations, p, Iterators.filter)
solve(p) = process_valuations(p, Iterators.filter)

# Predicates

"""
    is_commutative(::BinaryOperator)

See also [Binary Operators](@ref binary_operators).
"""
is_commutative(::union_typeof((∧, ⊼, ⊽, ∨, ⊻, ↔))) = true
is_commutative(::BinaryOperator) = false

"""
    is_associative(::BinaryOperator)

See also [Binary Operators](@ref binary_operators).
"""
is_associative(::union_typeof((∧, ∨, ⊻, ↔))) = true
is_associative(::BinaryOperator) = false

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

See also [Nullary Operators](@ref nullary_operators) and [`Proposition`](@ref).

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
p::Bool == q::Proposition = p ↔ is_tautology(q)
p::NullaryOperator == q::Proposition = Bool(p) == q
p::Proposition == q::Union{Bool, NullaryOperator} = q == p
p::Proposition == q::Proposition = is_tautology(p ↔ q)

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

Returns a boolean on whether `p` is a
[nullary operator](@ref nullary_operators).

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
(not logically equivalent to a [nullary operator](@ref nullary_operators)).

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

# Properties

"""
    dual(::Operator)

Returns the [operator](@ref operators_operators) that is the [dual]
(https://en.wikipedia.org/wiki/Boolean_algebra#Duality_principle)
of the given operator.

# Examples
```jldoctest
julia> dual(and)
or (generic function with 17 methods)

julia> @atomize and(p, q) == not(dual(and)(not(p), not(q)))
true

julia> dual(imply)
not_converse_imply (generic function with 3 methods)

julia> @atomize imply(p, q) == not(dual(imply)(not(p), not(q)))
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
    converse(::Operator)

Returns the [operator](@ref operators_operators) that is the
[converse](https://en.wikipedia.org/wiki/Converse_(logic))
of the given operator.

# Examples
```jldoctest
julia> converse(and)
and (generic function with 17 methods)

julia> @atomize and(p, q) == converse(and)(q, p)
true

julia> converse(imply)
converse_imply (generic function with 3 methods)

julia> @atomize imply(p, q) == converse(imply)(q, p)
true
```
"""
converse(co::Union{filter(O -> is_commutative(O.instance), uniontypes(BinaryOperator))...}) = co

eval_doubles(:converse, ((→, ←), (↛, ↚)))

"""
    left_neutrals(::Operator)

Return a `Set` of the [Nullary Operators](@ref nullary_operators) that are
left neutral elements of the given [operator](@ref operators_operators).

# Examples
```jldoctest
julia> left_neutrals(or)
Set{typeof(contradiction)} with 1 element:
  PAndQ.contradiction

julia> left_neutrals(imply)
Set{typeof(tautology)} with 1 element:
  PAndQ.tautology

julia> left_neutrals(nor)
Set{Union{typeof(contradiction), typeof(tautology)}}()
```
"""
left_neutrals(::union_typeof((∧, ↔, →))) = Set((⊤,))
left_neutrals(::union_typeof((∨, ⊻, ↚))) = Set((⊥,))
left_neutrals(::Operator) = Set{NullaryOperator}()

"""
    right_neutrals(::Operator)

Return a `Set` of the [Nullary Operators](@ref nullary_operators) that are
right neutral elements of the given [operator](@ref operators_operators).

# Examples
```jldoctest
julia> right_neutrals(or)
Set{typeof(contradiction)} with 1 element:
  PAndQ.contradiction

julia> right_neutrals(converse_imply)
Set{typeof(tautology)} with 1 element:
  PAndQ.tautology

julia> left_neutrals(nor)
Set{Union{typeof(contradiction), typeof(tautology)}}()
```
"""
right_neutrals(::union_typeof((∧, ↔, ←))) = Set((⊤,))
right_neutrals(::union_typeof((∨, ⊻, ↛))) = Set((⊥,))
right_neutrals(::Operator) = Set{NullaryOperator}()

# Operators

## Bool

"""
    Bool(::NullaryOperator)

See also [Nullary Operators](@ref nullary_operators).
"""
Bool(no::NullaryOperator) = convert(Bool, no)

¬p::Bool = !p
p::Union{Bool, NullaryOperator} ∧ q::Bool = Bool(p) && q
p::Union{Bool, NullaryOperator} ∨ q::Bool = Bool(p) || q
p::Bool ∧ q::NullaryOperator = q ∧ p
p::Bool ∨ q::NullaryOperator = q ∨ p

## Operators

### NullaryOperators

for no in (:⊤, :⊥)
    @eval $no() = $no
    @eval $no(valuation) = interpret(valuation, $no)
end

### Unary Operators

¬p::NullaryOperator = ¬Tree(p)

### Binary Operators

p::Union{NullaryOperator, Proposition} ∨ q::Union{NullaryOperator, Proposition} = ¬(p ⊽ q)

for (left, right) in (
    :⊼ => :(¬(p ∧ q)),
    :⊽ => :(¬p ∧ ¬q),
    :⊻ => :((p ∨ q) ∧ (p ⊼ q)),
) @eval begin
    $left(p::NullaryOperator, q::Bool) = $right
    $left(p::Bool, q::NullaryOperator) = $left(q, p)
    $left(p::Union{NullaryOperator, Proposition}, q::Union{NullaryOperator, Proposition}) =
        $right
end end

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

## Propositions

¬p::Atom = Literal(¬, p)
(¬p::Tree) = Tree(¬, p)
(¬p::Clause) = Clause(dual(nodevalue(p)), map(
    _child -> Literal(nodevalue(_child) == 𝒾 ? (¬) : 𝒾, child(_child)),
children(p)))
(¬p::Normal) = Normal(dual(nodevalue(p)), map(¬, children(p)))

p::Proposition ∧ q::Proposition = Normal(∧, p) ∧ Normal(∧, q)

for BO in uniontypes(BinaryOperator)
    bo = nameof(BO.instance)
    @eval $bo(p::Union{NullaryOperator, Proposition}) = Fix2($bo, p)
    @eval $bo(p::Union{NullaryOperator, Atom, Tree}, q::Union{NullaryOperator, Atom, Tree}) =
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

Literal(uo::UnaryOperator, p::Atom) = Tree(uo, p)

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
    convert(::Type{Bool}, ::NullaryOperator)

See also [Nullary Operators](@ref nullary_operators).
"""
convert(::Type{Bool}, ::typeof(⊤)) = true
convert(::Type{Bool}, ::typeof(⊥)) = false

"""
    convert(::Type{<:Proposition}, ::Union{NullaryOperator, Proposition})

See also [`NullaryOperator`](@ref) and [`Proposition`](@ref).
"""
convert(::Type{Atom}, p::Literal) = child(p)
convert(::Type{Literal}, p::Tree{<:UnaryOperator, <:Atom}) =
    Literal(nodevalue(p), child(p))
convert(::Type{Literal}, p::Atom) = Literal(𝒾, p)
convert(::Type{Tree}, p::NullaryOperator) = Tree(p)
convert(::Type{Tree}, p::Atom) = Tree(𝒾, p)
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
convert(::Type{Proposition}, no::NullaryOperator) = Tree(no)
