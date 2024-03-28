
import Base: Bool, Fix2, convert, promote_rule, ==, <
using Base.Iterators: product, repeated

# Truths

"""
    valuations(atoms)
    valuations(p)

Return an iterator of every possible [valuation]
(https://en.wikipedia.org/wiki/Valuation_(logic))
of the given `atoms` or the atoms contained in `p`.

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
        product(repeated([true, false], length(unique_atoms))...)
    )
end
valuations(p::Union{NullaryOperator, Proposition}) = valuations(collect(atoms(p)))

"""
    interpret(valuation, p)

Substitute each atom in `p` with values given by the `valuation`.

The `valuation` can be a `Function` that accepts an atom and returns a logical value,
a `Dict`ionary mapping from atoms to logical values, or an iterable that can construct such a dictionary.
No substitution is performed if an atom is not one of the dictionary's keys.

# Examples
```jldoctest
julia> @atomize interpret(atom -> ⊤, ¬p)
¬⊤

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

Return an `Array{Bool}` given by [`interpret`](@ref)ing
`p` with each of the [`valuations`](@ref).

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
    solutions(p; solver = Z3)

Return a stateful iterator of [`valuations`](@ref)
such that `interpret(valuation, p) == ⊤`.

To find every valuation that results in a true interpretation,
convert the proposition to conjunctive normal form using [`normalize`](@ref).
Otherwise, a subset of those valuations will be
identified using the [`tseytin`](@ref) transformation.

The `solver` can be either `Z3` or `PicoSAT`.

See also [`interpret`](@ref) and [`tautology`](@ref).

# Examples
```jldoctest
julia> @atomize solutions(p ∧ q)[1]
2-element Vector{PAndQ.Atom}:
 q
 p

julia> @atomize collect(only(solutions(p ∧ q)[2]))
2-element Vector{Bool}:
 1
 1
```
"""
function solutions(p; solver = Z3)
    clauses, atoms = _tseytin(p)
    valuations = solver.Solutions(clauses, length(atoms))
    _atoms, x = Atom[], Dict{Int, Int}()

    for (i, atom) in enumerate(atoms)
        if atom isa Constant || !startswith(string(atom.symbol), "##")
            push!(_atoms, atom)
            x[i] = length(x) + 1
        end
    end

    _atoms, Iterators.map(valuation -> map(last, Iterators.filter(
        ((atom, assignment),) -> get(x, atom, 0) != 0, enumerate(valuation))), valuations)
end

# Predicates

"""
    is_tautology(p)

Return a `Bool`ean indicating whether the given proposition
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
is_tautology(::Union{typeof(⊥), Atom}) = false
is_tautology(p) = is_contradiction(¬p)

"""
    is_contradiction(p)

Return a `Bool`ean indicating whether the given proposition
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
is_contradiction(p) = isempty(solutions(p)[2])

"""
    is_truth(p)

Return a `Bool`ean indicating whether given proposition is logically
equivalent to a [truth value](@ref nullary_operators).

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
is_truth(::Atom) = false
is_truth(p) = is_tautology(p) || is_contradiction(p)

"""
    is_contingency(p)

Return a `Bool`ean indicating whether `p` is a
[contingency](https://en.wikipedia.org/wiki/Contingency_(philosophy))
(not logically equivalent to a [truth value](@ref nullary_operators)).

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

Return a `Bool`ean indicating whether `p` is
[satisfiable](https://en.wikipedia.org/wiki/Satisfiability)
(not logically equivalent to a [`contradiction`](@ref)).

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

Return a `Bool`ean indicating whether `p` is
[falsifiable](https://en.wikipedia.org/wiki/Falsifiability)
(not logically equivalent to a [`tautology`](@ref)).

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

Return a `Bool`ean indicating whether the predicate [`is_satisfiable`](@ref)
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
    ==(p, q)
    p == q

Return a `Bool`ean indicating whether `p` and `q` are [logically equivalent]
(https://en.wikipedia.org/wiki/Logical_equivalence).

Constants are equivalent only if their [`value`](@ref)s are equivalent.

!!! info
    The `≡` symbol is sometimes used to represent logical equivalence.
    However, Julia uses `≡` as an alias for the builtin function `===`
    which cannot have methods added to it.

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
    <(p, q)
    p < q

Return a `Bool`ean indicating whether the arguments are ordered such that
`r < s < t`, where `r`, `s`, and `t` satisfy [`is_contradiction`](@ref),
[`is_contingency`](@ref), and [`is_tautology`](@ref), respectively.

# Examples
```jldoctest
julia> @atomize ⊥ < p < ⊤
true

julia> @atomize p ∧ ¬p < p < p ∨ ¬p
true

julia> @atomize p < p
false

julia> ⊤ < ⊥
false
```
"""
p::Bool < q::Union{NullaryOperator, Proposition} = p ? false : is_satisfiable(q)
p::NullaryOperator < q::Union{Bool, NullaryOperator, Proposition} = Bool(p) < q
p::Proposition < q::Union{Bool, NullaryOperator} = ¬q < ¬p
p::Proposition < q::Proposition =
    is_contradiction(p) ? is_satisfiable(q) : is_falsifiable(p) && is_tautology(q)

# Operators

"""
    Bool(truth_value)

Return a `Bool`ean corresponding to the given [truth value](@ref nullary_operators).

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

Atom(p) = convert(Atom, p)
AbstractSyntaxTree(p) = convert(AbstractSyntaxTree, p)

# Utilities

convert(::Type{Bool}, ::typeof(⊤)) = true
convert(::Type{Bool}, ::typeof(⊥)) = false

"""
    convert(::Type{<:Proposition}, p)

See also [`Proposition`](@ref).
"""
convert(::Type{AbstractSyntaxTree}, p::NullaryOperator) = AbstractSyntaxTree(p, Union{}[])
convert(::Type{AbstractSyntaxTree}, p::Atom) = AbstractSyntaxTree(𝒾, [p])
convert(::Type{Proposition}, p::NullaryOperator) = AbstractSyntaxTree(p)

"""
    promote_rule
"""
promote_rule(::Type{Bool}, ::Type{<:NullaryOperator}) = Bool
promote_rule(::Type{<:Atom}, ::Type{<:Atom}) = Atom
promote_rule(::Type{NullaryOperator}, ::Type{Proposition}) = AbstractSyntaxTree
promote_rule(::Type{Proposition}, ::Type{Proposition}) = AbstractSyntaxTree

# Interface Implementation

"""
    eval_pairs(f, pairs)

Define `f(::typeof(left)) = right` and `f(::typeof(right)) = left` for each pair `left` and `right` in `pairs`.
"""
eval_pairs(f, pairs) = for pair in pairs
    for (left, right) in (pair, reverse(pair))
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
    @eval symbol(::typeof($o)) = $(string(o))
end

Associativity(::union_typeof((∧, ↑, ↓, ∨, ↮, ↔, →, ↚))) = Left
Associativity(::union_typeof((↛, ←))) = Right

dual(o::UnaryOperator) = o
eval_pairs(:dual, (
    (⊤, ⊥),
    (∧, ∨),
    (↑, ↓),
    (↔, ↮),
    (→, ↚),
    (←, ↛)
))

converse(o::union_typeof((∧, ∨, ↑, ↓, ↔, ↮))) = o
eval_pairs(:converse, ((→, ←), (↛, ↚)))

is_commutative(::union_typeof((∧, ↑, ↓, ∨, ↮, ↔))) = true
is_commutative(::union_typeof((→, ↛, ←, ↚))) = false

is_associative(::union_typeof((∧, ∨, ↮, ↔))) = true
is_associative(::union_typeof((↑, ↓, →, ↛, ←, ↚))) = false

evaluate_not(::typeof(¬), ps) = only(ps)
evaluate_not(o, ps) = AbstractSyntaxTree(dual(o), map(¬, ps))

____evaluate_and_or(ao, o::NullaryOperator, ps, q) = _evaluate(ao, o, q)
____evaluate_and_or(ao, o, ps, q) = ao(q, AbstractSyntaxTree(o, ps))

___evaluate_and_or(ao, p::Atom, q) = ao(q, p)
___evaluate_and_or(ao, p, q) = ____evaluate_and_or(ao, deconstruct(p)..., q)

__evaluate_and_or(ao, o::NullaryOperator, ps, q) = _evaluate(ao, o, q)
__evaluate_and_or(ao, o, ps, q) = ___evaluate_and_or(ao, q, AbstractSyntaxTree(o, ps))

_evaluate_and_or(ao, p::Atom, q) = ___evaluate_and_or(ao, q, p)
_evaluate_and_or(ao, p, q) = __evaluate_and_or(ao, deconstruct(p)..., q)

evaluate_and_or(::typeof(∧), ::typeof(⊤), q) = q
evaluate_and_or(::typeof(∧), ::typeof(⊥), q) = ⊥
evaluate_and_or(::typeof(∨), ::typeof(⊤), q) = ⊤
evaluate_and_or(::typeof(∨), ::typeof(⊥), q) = q
evaluate_and_or(ao, p, q) = _evaluate_and_or(ao, p, q)

_evaluate(o::NullaryOperator) = o
_evaluate(::typeof(𝒾), p) = p
_evaluate(::typeof(¬), p::Bool) = !p
_evaluate(::typeof(¬), p::NullaryOperator) = dual(p)
_evaluate(::typeof(¬), p::Atom) = ¬p
_evaluate(::typeof(¬), p::AbstractSyntaxTree) = evaluate_not(nodevalue(p), children(p))
_evaluate(::typeof(∧), p::Bool, q::Bool) = p && q
_evaluate(::typeof(∨), p::Bool, q::Bool) = p || q
_evaluate(o::AndOr, p, q) = evaluate_and_or(o, p, q)
_evaluate(::typeof(→), p, q) = ¬p ∨ q
_evaluate(::typeof(↮), p, q) = (p ∨ q) ∧ (p ↑ q)
_evaluate(::typeof(←), p, q) = p ∨ ¬q
_evaluate(::typeof(↑), p, q) = ¬(p ∧ q)
_evaluate(::typeof(↓), p, q) = ¬p ∧ ¬q
_evaluate(::typeof(↛), p, q) = p ∧ ¬q
_evaluate(::typeof(↔), p, q) = (p ∧ q) ∨ (p ↓ q)
_evaluate(::typeof(↚), p, q) = ¬p ∧ q

function dispatch(f, o, ps)
    _arity = arity(o)
    _arity == length(ps) || error("write this error")

    if _arity == 0 f(o)
    elseif _arity == 1 f(o, only(ps))
    else f(o, first(ps), last(ps))
    end
end

evaluate(o::Union{NullaryOperator, UnaryOperator, BinaryOperator}, ps) = dispatch(_evaluate, o, ps)
evaluate(::typeof(⋀), ps) = fold(𝒾, (∧) => ps)
evaluate(::typeof(⋁), ps) = fold(𝒾, (∨) => ps)

___evaluation(::Eager, o, ps) = evaluate(o, ps)
___evaluation(::Lazy, o, ps) = _evaluation(o, ps)

__evaluation(::typeof(𝒾), ps) = ¬only(ps)
__evaluation(o, ps) = AbstractSyntaxTree(¬, [AbstractSyntaxTree(o, ps)])

function _evaluation(::typeof(¬), ps::Vector{AbstractSyntaxTree})
    q = only(ps)
    __evaluation(nodevalue(q), children(q))
end
_evaluation(o::UnaryOperator, ps::Vector{<:Atom}) = AbstractSyntaxTree(o, [only(ps)])
_evaluation(o, ps::Vector{AbstractSyntaxTree}) = AbstractSyntaxTree(o, ps)
_evaluation(o, ps) = _evaluation(o, map(AbstractSyntaxTree, ps))

Evaluation(::Union{NullaryOperator, typeof(𝒾), NaryOperator}) = Eager
Evaluation(::Union{typeof(¬), BinaryOperator}) = Lazy

evaluation(o, ps::Vector{Bool}) = evaluate(o, ps)
evaluation(o, ps) = ___evaluation(Evaluation(o)(), o, ps)

(o::Operator)(ps...) = evaluation(o, [ps...])

__print_expression(io, o, ps) = _show(print_proposition, io, ps) do io
    print(io, " ")
    show(io, "text/plain", o)
    print(io, " ")
end

_print_expression(io, p::NullaryOperator) = print_proposition(io, p)
_print_expression(io, ::typeof(𝒾), p) = print_proposition(io, p)
function _print_expression(io, ::typeof(¬), p)
    show(io, "text/plain", ¬)
    print_proposition(io, p)
end
function _print_expression(io, o::BinaryOperator, p, q)
    rs, stack = AbstractSyntaxTree[], AbstractSyntaxTree[]

    if is_associative(o)
        push!(stack, q, p)

        while !isempty(stack)
            r = pop!(stack)
            nodevalue(r) == o ? append!(stack, reverse(children(r))) : push!(rs, r)
        end
    else push!(rs, p, q)
    end

    __print_expression(io, o, rs)
end

print_expression(io, o::Union{NullaryOperator, UnaryOperator, BinaryOperator}, ps) =
    dispatch((_o, qs...) -> _print_expression(io, _o, qs...), o, ps)

_print_proposition(io, p::NullaryOperator) = show(io, "text/plain", p)
function _print_proposition(io, p::Constant)
    print(io, "\$(")
    show(io, p.value)
    print(io, ")")
end
_print_proposition(io, p::Variable) = print(io, p.symbol)
_print_proposition(io, p::AbstractSyntaxTree) = print_expression(io, nodevalue(p), children(p))

print_proposition(io, p) = _print_proposition(IOContext(io, :root => false), p)
