
import Base: Bool, Fix2, convert, ==, <
using Base: Iterators.product, uniontypes
using .PicoSAT: Solutions

# Internals

"""
    neutral_operator(::NullaryOperator)

Return one of [`AndOr`](@ref) that is the neutral
element of the given [`NullaryOperator`](@ref).

# Examples
```jldoctest
julia> PAndQ.neutral_operator(⊤)
∧

julia> PAndQ.neutral_operator(⊥)
∨
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

"""
    combine(p, q)
"""
function combine(p, q)
    p_atoms, q_atoms = p.atoms, q.atoms
    atom_type = promote_type(eltype(p_atoms), eltype(q_atoms))
    mapping = Dict{atom_type, Int}(Iterators.map(reverse, pairs(p_atoms)))
    atoms = append!(atom_type[], p_atoms)

    atoms, p.clauses ∪ Iterators.map(
        clause -> Set(Iterators.map(clause) do literal
            atom = q_atoms[abs(literal)]
            sign(literal) * get!(mapping, atom) do
                push!(atoms, atom)
                lastindex(atoms)
            end
        end),
    q.clauses)
end

# Truths

"""
    valuations(atoms)
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

_map(f, p) = map(child -> map(f, child), children(p))

"""
    map(::Function, ::Union{NullaryOperator, Proposition})

Apply the given function to each [`Atom`](@ref) in the given argument.

Alternatively, propositions are callable with the function as an argument.

See also [Nullary Operators](@ref nullary_operators) and [`Proposition`](@ref).

# Examples
```jldoctest
julia> @atomize map(atom -> ⊤, p ↔ q)
⊤ ↔ ⊤

julia> @atomize map(atom -> \$(something(value(atom)) + 1), \$1 ∧ \$2)
\$(2) ∧ \$(3)
```
"""
map(f, p::Atom) = f(p)
map(f, p::Union{NullaryOperator, Tree}) = nodevalue(p)(_map(f, p)...)
map(f, p::Union{Clause, Normal}) = fold(𝒾, nodevalue(p) => _map(f, p))

"""
    interpret(valuation, p)

Substitute each [`Atom`](@ref) in the given
[`Proposition`](@ref) with values from the `valuation`.

The `valuation` can be a `Function` with the signature
`valuation(::Atom)::Union{Bool, NullaryOperator, Proposition}`,
a `Dict`, or an iterable that can construct a `Dict`.
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
interpret(valuation::Function, p) = map(valuation, p)
interpret(valuation::Dict, p) = interpret(a -> get(valuation, a, a), p)
interpret(valuation, p) = interpret(Dict(valuation), p)

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
interpretations(valuations, p) = Iterators.map(valuation -> interpret(valuation, p), valuations)
interpretations(p) = Iterators.map(valuation -> Bool(interpret(a -> Dict(valuation)[a], normalize(¬, p))), valuations(p))

"""
    solutions(p)

Return a stateful iterator of [`valuations`](@ref)
such that `interpret(valuation, p) == ⊤`.

To find every valuation that results in a true interpretation,
convert the proposition to conjunctive normal form using [`normalize`](@ref).
Otherwise, a subset of those valuations will be
identified using the [`tseytin`](@ref) transformation.

See also [`interpret`](@ref) and [`tautology`](@ref).

# Examples
```jldoctest
julia> map(collect, solutions(⊤))
1-element Vector{Vector{Any}}:
 []

julia> @atomize map(collect, solutions(p))
1-element Vector{Vector{Pair{PAndQ.Variable, Bool}}}:
 [PAndQ.Variable(:p) => 1]

julia> map(collect, solutions(⊥))
Any[]
```
"""
function solutions(p::Normal{typeof(∧)})
    atoms = p.atoms
    Iterators.map(solution -> Iterators.map(
        literal -> atoms[abs(literal)] => !signbit(literal), solution), Solutions(p.clauses))
end
function solutions(p)
    q, rs = flatten(p)
    Iterators.map(solution -> Iterators.filter(
        ((atom, _),) -> atom isa Constant || !startswith(string(atom.symbol), "##"),
    solution), solutions(q ∧ normalize(∧, fold(tseytin, (∧) => rs))))
end

# Predicates

"""
    is_commutative(binary_operator)

Return a boolean indicating whether the given [binary operator](@ref binary_operators) has the
[commutative property](https://en.wikipedia.org/wiki/Commutative_property).

# Examples
```jldoctest
julia> is_commutative(∧)
true

julia> is_commutative(→)
false
```
"""
is_commutative(::union_typeof((∧, ↑, ↓, ∨, ↮, ↔))) = true
is_commutative(::BinaryOperator) = false

"""
    is_associative(binary_operator)

Return a boolean indicating whether the given [binary operator](@ref binary_operators) has the
[associative property](https://en.wikipedia.org/wiki/Associative_property).

# Examples
```jldoctest
julia> is_associative(∧)
true

julia> is_associative(→)
false
```
"""
is_associative(::union_typeof((∧, ∨, ↮, ↔))) = true
is_associative(::BinaryOperator) = false

"""
    is_tautology(p)

Return a boolean indicating whether the given proposition
is logically equivalent to a [`tautology`](@ref).

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
is_tautology(p) = is_contradiction(¬p)

"""
    is_contradiction(p)

Return a boolean indicating whether the given proposition
is logically equivalent to a [`contradiction`](@ref).

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
is_contradiction(p) = isempty(solutions(p))

"""
    is_truth(p)

Return a boolean indicating whether given proposition is logically
equivalent to a [nullary operator](@ref nullary_operators).

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
is_truth(p) = is_tautology(p) || is_contradiction(p)

"""
    is_contingency(p)

Return a boolean indicating whether `p` is a
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

Return a boolean indicating whether `p` is
[satisfiable](https://en.wikipedia.org/wiki/Satisfiability)
(not logically equivalent to a [`contradiction`](@ref)).

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

Returns a boolean indicating whether `p` is
[falsifiable](https://en.wikipedia.org/wiki/Falsifiability)
(not logica equivalent to a [`tautology`](@ref)).

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

"""
    is_equisatisfiable(p, q)

Return a boolean indicating whether the predicate [`is_satisfiable`](@ref)
is congruent for both propositions.

# Examples
```jldoctest
julia> is_equisatisfiable(⊤, ⊥)
false

julia> @atomize is_equisatisfiable(p, q)
true
```
"""
is_equisatisfiable(p, q) = is_satisfiable(p) == is_satisfiable(q)

## Ordering

"""
    ==(::Union{Bool, NullaryOperator, Proposition}, ::Union{Bool, NullaryOperator, Proposition})
    p == q

Return a boolean indicating whether `p` and `q` are [logically equivalent]
(https://en.wikipedia.org/wiki/Logical_equivalence).

[`Constant`](@ref)s are equivalent only if their values are equivalent.

!!! info
    The `≡` symbol is sometimes used to represent logical equivalence.
    However, Julia uses `≡` as an alias for the builtin function `===`
    which cannot have methods added to it.

See also [Nullary Operators](@ref nullary_operators) and [`Proposition`](@ref).

# Examples
```jldoctest
julia> @atomize ⊥ == p ∧ ¬p
true

julia> @atomize (p ↔ q) == ¬(p ↮ q)
true

julia> @atomize \$1 == \$1
true

julia> @atomize p == ¬p
false
```
"""
p::Constant == q::Constant = p.value == q.value
p::Variable == q::Variable = p === q
p::Atom == q::Atom = false
p::Bool == q::Union{NullaryOperator, Proposition} = (p ? is_tautology : is_contradiction)(q)
p::NullaryOperator == q::Union{Bool, Proposition} = Bool(p) == q
p::Proposition == q::Union{Bool, NullaryOperator} = q == p
p::Proposition == q::Proposition = is_contradiction(p ↮ q)

"""
    <(::Union{Bool, NullaryOperator, Proposition}, ::Union{Bool, NullaryOperator, Proposition})

Return a boolean indicating whether the arguments are ordered such that
`p < q < r`, where `p`, `q`, and `r` satisfy [`is_contradiction`](@ref),
[`is_contingency`](@ref), and [`is_tautology`](@ref), respectively.

See also [Nullary Operators](@ref nullary_operators) and [`Proposition`](@ref).

# Examples
```jldoctest
julia> @atomize ⊥ < p < ⊤
true

julia> @atomize p ∧ ¬p < p < p ∨ ¬p
true

julia> ⊤ < ⊥
false

julia> @atomize p < p
false
```
"""
p::NullaryOperator < q::NullaryOperator = p == ⊥ && q == ⊤
::Union{Atom, Literal} < ::Union{Atom, Literal} = false
p::Bool < q::Union{NullaryOperator, Proposition} = p ? false : is_satisfiable(q)
p::NullaryOperator < q::Union{Bool, Proposition} = Bool(p) < q
p::Proposition < q::Union{Bool, NullaryOperator} = ¬q < ¬p
<(p::Proposition, q::Proposition) =
    is_contradiction(p) ? is_satisfiable(q) : !is_tautology(p) && is_tautology(q)

# Properties

"""
    dual(operator)

Returns the [operator](@ref operators_operators) that is the [dual]
(https://en.wikipedia.org/wiki/Boolean_algebra#Duality_principle)
of the given [operator](@ref operators_operators).

# Examples
```jldoctest
julia> dual(and)
∨

julia> @atomize and(p, q) == not(dual(and)(not(p), not(q)))
true

julia> dual(imply)
↚

julia> @atomize imply(p, q) == not(dual(imply)(not(p), not(q)))
true
```
"""
dual(unary_operator::UnaryOperator) = unary_operator

eval_doubles(:dual, (
    (⊤, ⊥),
    (∧, ∨),
    (↑, ↓),
    (↔, ↮),
    (→, ↚),
    (←, ↛)
))

"""
    converse(binary_operator)

Returns the [operator](@ref operators_operators) that is the
[converse](https://en.wikipedia.org/wiki/Converse_(logic))
of the given [binary operator](@ref binary_operators).

# Examples
```jldoctest
julia> converse(and)
∧

julia> @atomize and(p, q) == converse(and)(q, p)
true

julia> converse(imply)
←

julia> @atomize imply(p, q) == converse(imply)(q, p)
true
```
"""
converse(binary_operator::union_typeof((∧, ∨, ↑, ↓, ↔, ↮))) = binary_operator

eval_doubles(:converse, ((→, ←), (↛, ↚)))

# Operators

## Bool

"""
    Bool(nullary_operator)

Return a `Bool` corresponding to the given [nullary operator](@ref nullary_operators).

# Examples
```jldoctest
julia> Bool(⊤)
true

julia> Bool(⊥)
false
```
"""
Bool(nullary_operator::NullaryOperator) = convert(Bool, nullary_operator)

(::typeof(¬))(p::Bool) = !p
(::typeof(∧))(p::Bool, q::Bool) = p && Bool(q)
(::typeof(∨))(p::Bool, q::Bool) = p || Bool(q)
(o::union_typeof((∧, ∨)))(p::Bool, q::NullaryOperator) = o(p, Bool(q))
(o::union_typeof((∧, ∨)))(p::NullaryOperator, q::Bool) = o(q, p)

## Operators

### NullaryOperators

(o::union_typeof((⊤, ⊥)))() = o

### Unary Operators

(::typeof(¬))(p::NullaryOperator) = ¬Tree(p)

### Binary Operators

(::typeof(∨))(p::Some{<:NullaryOperator}, q::Atom) = normalize(¬, ¬(p ↓ q))
(::typeof(∨))(
    p::Union{NullaryOperator, Some{<:NullaryOperator}, Proposition},
    q::Union{NullaryOperator, Some{<:NullaryOperator}, Proposition}
) = ¬(p ↓ q)

for (left, right) in (
    :not_and => :(¬(p ∧ q)),
    :not_or => :(¬p ∧ ¬q),
    :exclusive_or => :((p ∨ q) ∧ (p ↑ q)),
    :not_exclusive_or => :((p ∧ q) ∨ (p ↓ q)),
    :not_imply => :(p ∧ ¬q),
    :imply => :(¬p ∨ q),
    :not_converse_imply => :(¬p ∧ q),
    :converse_imply => :(p ∨ ¬q)
) @eval begin
    function negated_normal!(operator_stack, input_stack, output_stack, node::Tree{Operator{$(QuoteNode(left))}})
        p, q = node.nodes
        operator_stack, push!(input_stack, $right), output_stack
    end
    PAndQ.$left(p::Normal, q::Normal) = $right
    PAndQ.$left(p::Some{<:NullaryOperator}, q::Atom) = normalize(¬, $right)
    PAndQ.$left(
        p::Union{Bool, Some{<:NullaryOperator}, NullaryOperator, Proposition},
        q::Union{Bool, Some{<:NullaryOperator}, NullaryOperator, Proposition}
    ) = $right
end end

## Propositions

(::typeof(𝒾))(p) = p
(::typeof(¬))(::Some{typeof(⊤)}) = Some(⊥)
(::typeof(¬))(::Some{typeof(⊥)}) = Some(⊤)
(::typeof(¬))(p::Union{Atom, Tree}) = Tree(¬, p)
(::typeof(¬))(p::Normal) =
    Normal(dual(nodevalue(p)), p.atoms, Set(Iterators.map(clause -> Set(Iterators.map(-, clause)), p.clauses)))

(::typeof(∧))(::Some{typeof(⊤)}, q::Union{Bool, Some{<:NullaryOperator}, NullaryOperator, Proposition}) = q
(::typeof(∧))(p::Some{typeof(⊥)}, q::Union{Bool, Some{<:NullaryOperator}, NullaryOperator, Proposition}) = p
(::typeof(∧))(p::Union{Bool, NullaryOperator, Proposition}, q::Some{<:NullaryOperator}) = q ∧ p

for bo in (:←, :∧, :∨, :→, :↮, :↑, :↓, :↛, :↔, :↚) @eval begin
    PAndQ.$bo(p::Union{Some{<:NullaryOperator}, NullaryOperator, Proposition}) = Fix2($bo, p)
    PAndQ.$bo(p::Union{NullaryOperator, Atom, Tree}, q::Union{NullaryOperator, Atom, Tree}) =
        Tree($bo, Tree(p), Tree(q))
    PAndQ.$bo(p::Normal, q::Union{NullaryOperator, Proposition}) = $bo(Tree(p), q)
    PAndQ.$bo(p::Union{NullaryOperator, Proposition}, q::Normal) = $bo(p, Tree(q))
end end

(::typeof(∧))(p::Normal{typeof(∧)}, q::Normal{typeof(∧)}) = Normal(∧, combine(p, q)...)
(::typeof(∨))(p::Normal{typeof(∨)}, q::Normal{typeof(∨)}) = Normal(∨, combine(p, q)...)
(::typeof(∧))(p::Normal, q::Normal) = Normal(∧, p) ∧ Normal(∧, q)
(::typeof(∨))(p::Normal, q::Normal) = Normal(∨, p) ∨ Normal(∨, q)

# Constructors

Literal(uo, p::Atom) = Tree(uo, p)

for P in (:Atom, :Literal, :Tree)
    @eval $P(p) = convert($P, p)
end

Normal(::AO, p) where AO = convert(Normal{AO}, p)

# Utilities

convert(::Type{Bool}, p::Some{<:NullaryOperator}) = Bool(something(p))
convert(::Type{Bool}, ::typeof(⊤)) = true
convert(::Type{Bool}, ::typeof(⊥)) = false
convert(::Type{Bool}, p::Tree{<:NullaryOperator}) = Bool(nodevalue(p))

"""
    convert(::Type{<:Proposition}, p)

See also [`Proposition`](@ref).
"""
convert(::Type{Atom}, p::Literal{typeof(𝒾)}) = child(p)
convert(::Type{Literal}, p::Atom) = Tree(p)
convert(::Type{Tree}, p::NullaryOperator) = Tree(p)
convert(::Type{Tree}, p::Atom) = Tree(𝒾, p)
convert(::Type{Tree}, p::Normal) = map(𝒾, p)
convert(::Type{Normal{AO}}, p::Union{NullaryOperator, Proposition}) where AO =
    normalize(AO.instance, p)
convert(::Type{Proposition}, p::NullaryOperator) = Tree(p)
convert(P::Type{<:Proposition}, p::Some{<:NullaryOperator}) = convert(P, something(p))
