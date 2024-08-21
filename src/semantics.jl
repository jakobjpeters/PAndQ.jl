
import Base: Bool, Fix2, ==, <, convert, hash, promote_rule
using Base.Iterators: product, repeated
using Base: current_project

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
2-element Vector{Vector{Pair{PAndQ.AbstractSyntaxTree, Bool}}}:
 [PAndQ.AbstractSyntaxTree(:p) => 1]
 [PAndQ.AbstractSyntaxTree(:p) => 0]

julia> @atomize collect(valuations(p ∧ q))
2×2 Matrix{Vector{Pair{PAndQ.AbstractSyntaxTree, Bool}}}:
 [AbstractSyntaxTree(:p)=>1, AbstractSyntaxTree(:q)=>1]  …  [AbstractSyntaxTree(:p)=>1, AbstractSyntaxTree(:q)=>0]
 [AbstractSyntaxTree(:p)=>0, AbstractSyntaxTree(:q)=>1]     [AbstractSyntaxTree(:p)=>0, AbstractSyntaxTree(:q)=>0]
```
"""
function valuations(atoms)
    unique_atoms = unique(atoms)
    Iterators.map(valuation -> map(Pair, unique_atoms, valuation),
        product(repeated([true, false], length(unique_atoms))...)
    )
end
valuations(p::Union{NullaryOperator, AbstractSyntaxTree}) = valuations(collect(atoms(p)))

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

julia> @atomize interpret([p => ⊤], p ∧ q)
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
interpretations(p) = Iterators.map(valuation -> Bool(interpret(valuation, normalize(¬, p))), valuations(p))

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
2-element Vector{PAndQ.AbstractSyntaxTree}:
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
    _atoms, x = AbstractSyntaxTree[], Dict{Int, Int}()

    for (i, atom) in enumerate(atoms)
        value = atom.value
        if value isa Some || !startswith(string(value), "##")
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
is_tautology(::typeof(⊥)) = false
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
p::AbstractSyntaxTree == ::typeof(⊤) = is_tautology(p)
p::AbstractSyntaxTree == ::typeof(⊥) = is_contradiction(p)
p::NullaryOperator == q::AbstractSyntaxTree = q == p
p::AbstractSyntaxTree == q::AbstractSyntaxTree =
    all(r -> r.kind != operator, [p, q]) ? p.value == q.value : is_contradiction(p ↮ q)

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
::typeof(⊥) < ::typeof(⊤) = true
::NullaryOperator < ::NullaryOperator = false
p::AbstractSyntaxTree < ::typeof(⊤) = is_falsifiable(p)
p::AbstractSyntaxTree < ::typeof(⊥) = is_satisfiable(p)
p::NullaryOperator < q::AbstractSyntaxTree = q < p
p::AbstractSyntaxTree < q::AbstractSyntaxTree =
    is_contradiction(p) ? is_satisfiable(q) : is_falsifiable(p) && is_tautology(q)

"""
    hash(::Union{AbstractSyntaxTree, Operator}, ::UInt)

Return `zero(UInt)`.

Since `p == q` implies `hash(p) == hash(q)`, obtaining a better hash
value would require finding the [`solutions`](@ref) in some form.
Instead of using an NP-complete hash time, this instead opts for linear lookup time.
An [`Operator`](@ref) may be logically equivalent to a proposition, and so is also in this case.

# Examples

```jldoctest
julia> @atomize hash(p)
0x0000000000000000
```
"""
hash(::Union{AbstractSyntaxTree, Operator}, ::UInt) = zero(UInt)

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

AbstractSyntaxTree(p) = convert(AbstractSyntaxTree, p)

# Utilities

convert(::Type{Bool}, ::typeof(⊤)) = true
convert(::Type{Bool}, ::typeof(⊥)) = false

"""
    convert(::Type{<:AbstractSytnaxTree}, p)

See also [`AbstractSyntaxTree`](@ref).
"""
convert(::Type{AbstractSyntaxTree}, p::NullaryOperator) = AbstractSyntaxTree(operator, p)
convert(::Type{AbstractSyntaxTree}, p::Symbol) = AbstractSyntaxTree(variable, p)
convert(::Type{AbstractSyntaxTree}, p::Some) = AbstractSyntaxTree(constant, p)

"""
    promote_rule
"""
promote_rule(::Type{Bool}, ::Type{<:NullaryOperator}) = Bool
promote_rule(::Type{NullaryOperator}, ::Type{AbstractSyntaxTree}) = AbstractSyntaxTree

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

arity(o) = get(arities, name(o), nothing)

initial_value(::union_typeof((∧, ↔, →, ←))) = ⊤
initial_value(::union_typeof((∨, ↮, ↚, ↛))) = ⊥
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

is_commutative(o) = name(o) in commutativities

is_associative(o) = name(o) in associativities

Base.Bool(p::AbstractSyntaxTree) = p.kind == operator ? Bool(Operator{p.value::Symbol}()) : error()

evaluate(o, ps) =
    if o in [:tautology, :contradiction] AbstractSyntaxTree(operator, o)
    elseif o == :not
        q = only(ps)
        branches, o_q = q.branches, nodevalue(q)
        o_q == (¬) ? only(branches) : evaluate(name(dual(o_q)), map(¬, branches))
    elseif o in [:and, :or]
        q, r = ps
        _initial_value, o_q = initial_value(Operator{o}()), nodevalue(q)
        if o_q === _initial_value r
        else
            o_r = nodevalue(r)
            if o_r === _initial_value q
            else
                dual_initial_value = dual(_initial_value)
                any(o -> o === dual_initial_value, [o_q o_r]) ?
                    AbstractSyntaxTree(operator, dual_initial_value, AbstractSyntaxTree[]) :
                    AbstractSyntaxTree(operator, o, [q, r])
            end
        end
    elseif o == :conjunction fold(𝒾, (∧) => ps)
    elseif o == :disjunction fold(𝒾, (∨) => ps)
    else o in keys(evaluations) ?
        interpret(map(((i, p),) -> @atomize($i => p), enumerate(ps)), evaluations[o]) :
        evaluate(Operator{o}(), ps...)
    end

evaluation(::Eager, o, ps) = evaluate(name(o), ps)
evaluation(::Lazy, o, ps) = AbstractSyntaxTree(o, ps)

Evaluation(::Union{NullaryOperator, typeof(𝒾), NaryOperator}) = Eager
Evaluation(::Union{typeof(¬), BinaryOperator}) = Lazy

_evaluation(o, ps::Vector{AbstractSyntaxTree}) = evaluation(Evaluation(o)(), o, ps)
_evaluation(o, ps::Vector{Bool}) =
    if o == ¬; !only(ps)
    elseif o == ∧; all(ps)
    elseif o == ∨; any(ps)
    else evaluate(o, ps)
    end
_evaluation(o, ps) = _evaluation(o, map(AbstractSyntaxTree, ps))

(o::Operator)(ps::AbstractSyntaxTree...) = evaluation(Evaluation(o)(), o, [ps...])
(o::Operator)(ps::Bool...) = _evaluation(o, [ps...])
(o::Operator)() = _evaluation(o, AbstractSyntaxTree[])
(o::Operator)(ps...) = o(map(AbstractSyntaxTree, ps)...)

print_expression(io, o, ps) =
    if o in [:tautology, :contradiction, :not]
        ns, ss = print_dict[o]

        for (i, s) in enumerate(ss)
            if i in ns
                print_proposition(io, ps[parse(Int, s)])
            else print(io, s)
            end
        end
    elseif o in [:and, :or, :not_or, :not_and, :imply, :not_imply, :not_converse_imply, :exclusive_or, :not_exclusive_or, :converse_imply]
        _o, qs, stack = Operator{o}(), AbstractSyntaxTree[], AbstractSyntaxTree[]

        if is_associative(_o)
            append!(stack, ps)

            while !isempty(stack)
                q = pop!(stack)
                nodevalue(q) == _o ? append!(stack, (children(q))) : push!(qs, q)
            end

            reverse!(qs)
        else append!(qs, ps)
        end

        _show(print_proposition, io, qs) do io
            print(io, " ")
            show(io, MIME"text/plain"(), _o)
            print(io, " ")
        end
    else print_expression(io, Operator{o}(), ps)
    end

_print_proposition(io, p::AbstractSyntaxTree) =
    if p.kind == variable print(io, p.value::Symbol)
    elseif p.kind == constant
        print(io, "\$(")
        show(io, something(p.value))
        print(io, ")")
    else print_expression(io, name(nodevalue(p)), children(p))
    end

print_proposition(io, p) = _print_proposition(IOContext(io, :root => false), p)

function _is_commutative(o::Operator)
    p, q = map(o -> AbstractSyntaxTree(variable, o), (:p, :q))
    o(p, q) == o(q, p)
end

const p, q, r = @variables p q r

function __register_operator(name, _arity)
    if _arity == 2
        AbstractSyntaxTree(operator, name, [p, q]) == AbstractSyntaxTree(operator, name, [q, p]) && push!(commutativities, name)
        AbstractSyntaxTree(operator, name, [AbstractSyntaxTree(operator, name, [p, q]), r]) == AbstractSyntaxTree(operator, name, [p, AbstractSyntaxTree(operator, name, [q, r])]) && push!(associativities, name)
    end

    Operator{name}()
end

function _register_operator(name, evaluation)
    evaluations[name] = evaluation
    arities[name] = length(unique(atoms(evaluations)))
end

register_operator(name::Symbol, evaluation::AbstractSyntaxTree) =
    __register_operator(name, _register_operator(name, evaluation))

const arities = Dict(:tautology => 0, :contradiction => 0, :not => 1, :and => 2, :or => 2)
const evaluations = Dict{Symbol, AbstractSyntaxTree}()
const associativities, commutativities = Set([:and, :or]), Set([:and, :or])

const os_ps = map(((o, p),) -> name(o) => p, @atomize [
    𝒾 => $1,
    (→) => ¬$1 ∨ $2,
    (←) => $1 ∨ ¬$2,
    (↑) => ¬($1 ∧ $2),
    (↓) => ¬($1 ∨ $2),
    (↛) => $1 ∧ ¬$2,
    (↚) => ¬$1 ∧ $2,
    (↮) => ($1 ∨ $2) ∧ ($1 ↑ $2),
    (↔) => ($1 ∧ $2) ∨ ($1 ↓ $2)
])

for (o, p) in os_ps
    _register_operator(o, p)
end

for (o, _) in os_ps
    __register_operator(o, arity(Operator{o}()))
end

const print_dict = Dict(map(append!(
    Pair{Operator, String}[(¬) => "¬1"],
    map(o -> o => "$(symbol(o))", [⊤, ⊥]),
    map(o -> o => "1 $(symbol(o)) 2", [∧, ∨, →, ↮, ←, ↑, ↓, ↛, ↔, ↚]),
)) do (o, s)
    current, ns, ss = firstindex(s), Set{Int}(), SubstitutionString[]

    for match in eachmatch(r"(\d+)", s)
        capture, offset = only(match.captures), match.offset
        current < offset && push!(ss, s[current:prevind(s, offset)])
        push!(ss, capture[begin:end])
        push!(ns, length(ss))
        current = offset + ncodeunits(capture)
    end

    last = lastindex(s)
    current ≤ last && push!(ss, s[current:last])

    name(o), ns => ss
end)
