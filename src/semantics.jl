
import Base: Bool, Fix2, convert, promote_rule, ==, <
using Base: Iterators.product, uniontypes
using .PicoSAT: Solutions

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
solutions(p::Normal{typeof(∧)}) = Iterators.map(solution -> Iterators.map(
        literal -> p.atoms[abs(literal)] => !signbit(literal), solution), Solutions(p.clauses))
function solutions(p)
    q, rs = flatten(p)
    Iterators.map(solution -> Iterators.filter(
        ((atom, _),) -> atom isa Constant || !startswith(string(atom.symbol), "##"),
    solution), solutions(q ∧ normalize(∧, fold(tseytin, (∧) => rs))))
end

# Predicates

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
p::Bool < q::Union{NullaryOperator, Proposition} = p ? false : is_satisfiable(q)
p::NullaryOperator < q::Union{Bool, NullaryOperator, Proposition} = Bool(p) < q
p::Proposition < q::Union{Bool, NullaryOperator} = ¬q < ¬p
p::Proposition < q::Proposition =
    is_contradiction(p) ? is_satisfiable(q) : !is_tautology(p) && is_tautology(q)

# Operators

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
Bool(o::NullaryOperator) = convert(Bool, o)

# Constructors

Literal(uo, p::Atom) = Tree(uo, p)

Tree(::typeof(¬), p::Tree{typeof(𝒾)}) = Tree(¬, child(p))

for P in (:Atom, :Literal, :Tree)
    @eval $P(p) = convert($P, p)
end

Normal(::AO, p) where AO = convert(Normal{AO}, p)

# Utilities

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
convert(::Type{Tree}, p::Normal) = normalize(¬, map(𝒾, p))
convert(::Type{Normal{AO}}, p::Union{NullaryOperator, Proposition}) where AO =
    normalize(AO.instance, p)
convert(::Type{Proposition}, p::NullaryOperator) = Tree(p)

"""
    promote_rule
"""
promote_rule(::Type{Bool}, ::Type{<:NullaryOperator}) = Bool
promote_rule(::Type{<:Atom}, ::Type{<:Atom}) = Atom
promote_rule(::Type{<:Union{NullaryOperator, Proposition}}, ::Type{<:Union{NullaryOperator, Proposition}}) = Tree

# Interface Implementation

"""
    eval_doubles(f, doubles)
"""
eval_doubles(f, doubles) = for double in doubles
    for (left, right) in (double, reverse(double))
        @eval $f(::typeof($left)) = $right
    end
end

arity(::NullaryOperator) = 0
arity(::UnaryOperator) = 1
arity(::BinaryOperator) = 2

initial_value(::union_typeof((∧, ↔, →, ←))) = Some(⊤)
initial_value(::union_typeof((∨, ↮, ↚, ↛))) = Some(⊥)
initial_value(::union_typeof((↑, ↓))) = nothing

for o in (:⊤, :⊥, :𝒾, :¬, :∧, :↑, :↓, :∨, :↮, :↔, :→, :↛, :←, :↚, :⋀, :⋁)
    @eval symbol_of(::typeof($o)) = $(string(o))
end

Associativity(::union_typeof((∧, ↑, ↓, ∨, ↮, ↔, →, ↚))) = Left()
Associativity(::union_typeof((↛, ←))) = Right()

dual(o::UnaryOperator) = o
eval_doubles(:dual, (
    (⊤, ⊥),
    (∧, ∨),
    (↑, ↓),
    (↔, ↮),
    (→, ↚),
    (←, ↛)
))

converse(o::union_typeof((∧, ∨, ↑, ↓, ↔, ↮))) = o
eval_doubles(:converse, ((→, ←), (↛, ↚)))

is_commutative(::union_typeof((∧, ↑, ↓, ∨, ↮, ↔))) = true
is_commutative(::union_typeof((→, ↛, ←, ↚))) = false

is_associative(::union_typeof((∧, ∨, ↮, ↔))) = true
is_associative(::union_typeof((↑, ↓, →, ↛, ←, ↚))) = false

___evaluate(::typeof(∧), ::typeof(⊤), q) = q
___evaluate(::typeof(∧), ::typeof(⊥), q) = ⊥
___evaluate(::typeof(∨), ::typeof(⊤), q) = ⊤
___evaluate(::typeof(∨), ::typeof(⊥), q) = q
___evaluate(o, p::Tree{<:NullaryOperator}, q) = ___evaluate(o, nodevalue(p), q)

__evaluate(o, p::Union{NullaryOperator, Tree{<:NullaryOperator}}, q) = ___evaluate(o, p, q)
__evaluate(o, p, q) = o(p, q)

_evaluate(o, p::Union{NullaryOperator, Tree{<:NullaryOperator}}, q) = ___evaluate(o, p, q)
_evaluate(o, p, q) = __evaluate(o, q, p)

evaluate(o::NullaryOperator) = o
evaluate(::typeof(𝒾), p) = p
evaluate(::typeof(¬), p::Bool) = !p
evaluate(::typeof(¬), p::NullaryOperator) = dual(p)
evaluate(::typeof(¬), p::Atom) = ¬p
evaluate(::typeof(¬), p::Tree{typeof(𝒾)}) = ¬child(p)
evaluate(::typeof(¬), p::Tree{typeof(¬)}) = child(p)
evaluate(::typeof(¬), p::Tree) = dual(nodevalue(p))(map(¬, p.nodes)...)
evaluate(::typeof(¬), p::Normal) =
    Normal(dual(nodevalue(p)), p.atoms, Set(Iterators.map(clause -> Set(Iterators.map(-, clause)), p.clauses)))
evaluate(::typeof(∧), p::Bool, q::Bool) = p && q
evaluate(::typeof(∨), p::Bool, q::Bool) = p || q
function evaluate(o::AO, p::Normal{AO}, q::Normal{AO}) where AO <: AndOr
    p_atoms, q_atoms = p.atoms, q.atoms
    atom_type = promote_type(eltype(p_atoms), eltype(q_atoms))
    mapping = Dict{atom_type, Int}(Iterators.map(reverse, pairs(p_atoms)))
    atoms = append!(atom_type[], p_atoms)

    Normal(o, atoms, p.clauses ∪ Iterators.map(
        clause -> Set(Iterators.map(clause) do literal
            atom = q_atoms[abs(literal)]
            sign(literal) * get!(mapping, atom) do
                push!(atoms, atom)
                lastindex(atoms)
            end
        end),
    q.clauses))
end
evaluate(o::AndOr, p::Normal, q::Normal) = o(Normal(o, p), Normal(o, q))
evaluate(o::AndOr, p, q) = _evaluate(o, p, q)
evaluate(::typeof(→), p, q) = ¬p ∨ q
evaluate(::typeof(↮), p, q) = (p ∨ q) ∧ (p ↑ q)
evaluate(::typeof(←), p, q) = p ∨ ¬q
evaluate(::typeof(↑), p, q) = ¬(p ∧ q)
evaluate(::typeof(↓), p, q) = ¬p ∧ ¬q
evaluate(::typeof(↛), p, q) = p ∧ ¬q
evaluate(::typeof(↔), p, q) = (p ∧ q) ∨ (p ↓ q)
evaluate(::typeof(↚), p, q) = ¬p ∧ q
evaluate(::typeof(⋀), ps) = fold(𝒾, (∧) => ps)
evaluate(::typeof(⋁), ps) = fold(𝒾, (∨) => ps)

_evaluation(o::UnaryOperator, p::Atom) = Tree(o, p)
_evaluation(o, ps::Tree...) = Tree(o, ps...)
_evaluation(o, ps...) = _evaluation(o, map(Tree, ps)...)

evaluation(::Eager, o, ps...) = evaluate(o, ps...)
evaluation(::Lazy, o, ps...) = _evaluation(o, ps...)

__Evaluation(::Bool...) = Eager()
__Evaluation(ps...) = Lazy()

_Evaluation(::typeof(𝒾), p) = Eager()
_Evaluation(::typeof(¬), ::Normal) = Eager()
_Evaluation(::BinaryOperator, ::Normal, ::Normal) = Eager()
_Evaluation(::NaryOperator, ps) = Eager()
_Evaluation(o, ps...) = __Evaluation(ps...)

Evaluation(o::Union{NullaryOperator, UnaryOperator, BinaryOperator, NaryOperator}, ps...) =
    _Evaluation(o, ps...)

(o::Operator)(ps...) = evaluation(Evaluation(o, ps...), o, ps...)

_pretty_print(io, o, ps) = __show(show_proposition, io, ps) do io
    print(io, " ")
    show(io, MIME"text/plain"(), o)
    print(io, " ")
end

pretty_print(io, p::NullaryOperator) = show_proposition(io, p)
pretty_print(io, ::typeof(𝒾), p) = show_proposition(io, p)
function pretty_print(io, ::typeof(¬), p)
    show(io, MIME"text/plain"(), ¬)
    show_proposition(io, p)
end
pretty_print(io, o::BinaryOperator, p, q) = _pretty_print(io, o, (p, q))

_show_proposition(io, p::NullaryOperator) = show(io, MIME"text/plain"(), p)
function _show_proposition(io, p::Constant)
    print(io, "\$(")
    show(io, p.value)
    print(io, ")")
end
_show_proposition(io, p::Variable) = print(io, p.symbol)
_show_proposition(io, p::Tree) = pretty_print(io, nodevalue(p), children(p)...)
function _show_proposition(io, p::Union{Clause, Normal})
    o, qs = nodevalue(p), children(p)
    isempty(qs) ? pretty_print(io, something(initial_value(o))) : _pretty_print(io, o, qs)
end

show_proposition(io, p) = _show_proposition(IOContext(io, :root => false), p)
