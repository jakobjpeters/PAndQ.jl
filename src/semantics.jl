
import Base: Bool, convert, promote_rule
using Base.Iterators: product, repeated
using Base: operator_associativity

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

"""
    is_equivalent(p, q)

Return a `Bool`ean indicating whether `p` and `q` are [logically equivalent]
(https://en.wikipedia.org/wiki/Logical_equivalence).

Constants are equivalent only if their [`value`](@ref)s are equivalent.

!!! info
    The `≡` symbol is sometimes used to represent logical equivalence.
    However, Julia uses `≡` as an alias for the builtin function `===`
    which cannot have methods added to it.

# Examples
```jldoctest
julia> @atomize is_equivalent(⊥, p ∧ ¬p)
true

julia> @atomize is_equivalent(p ↔ q, ¬(p ↮ q))
true

julia> @atomize is_equivalent(\$1, \$1)
true

julia> @atomize is_equivalent(p, ¬p)
false
```
"""
is_equivalent(p::AbstractSyntaxTree, ::typeof(⊤)) = is_tautology(p)
is_equivalent(p::AbstractSyntaxTree, ::typeof(⊥)) = is_contradiction(p)
is_equivalent(p::NullaryOperator, q::AbstractSyntaxTree) = is_equivalent(q, p)
function is_equivalent(p::AbstractSyntaxTree, q::AbstractSyntaxTree)
    kinds = [p.kind, q.kind]
    if all(==(variable), kinds) p == q
    elseif all(==(constant), kinds) p.value == q.value
    else is_contradiction(p ↮ q)
    end
end

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

arity(o) = get(arities, name(o), nothing)

initial_value(o) = name(o) in keys(initial_values) ? Operator{initial_values[name(o)].value::Symbol}() : nothing

for o in (:⊤, :⊥, :𝒾, :¬, :∧, :↑, :↓, :∨, :↮, :↔, :→, :↛, :←, :↚, :⋀, :⋁)
    @eval symbol(::typeof($o)) = $(string(o))
end

dual(o) = Operator{get(duals, name(o)) do
    _arity = arity(o)
    name(register_operator(gensym("dual_$(name(o))"), _arity, map(¬, ¬AbstractSyntaxTree(
        operator, name(o), map(i -> @atomize($i), 1:_arity)))))
end}()

# inverse, contrapositive, converse

is_commutative(o) = name(o) in commutatives

is_associative(o) = name(o) in associatives

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
        if o_q == _initial_value r
        else
            o_r = nodevalue(r)
            if o_r == _initial_value q
            else
                dual_initial_value = dual(_initial_value)
                any(o -> o == dual_initial_value, [o_q o_r]) ?
                    AbstractSyntaxTree(operator, name(dual_initial_value)) :
                    AbstractSyntaxTree(operator, o, [q, r])
            end
        end
    elseif o == :conjunction fold(𝒾, (∧) => ps)
    elseif o == :disjunction fold(𝒾, (∨) => ps)
    else o in keys(evaluations) ?
        interpret(map(((i, p),) -> @atomize($i => p), enumerate(ps)), evaluations[o]) :
        evaluate(Operator{o}(), ps...)
    end

function evaluation(o, ps)
    _name = name(o)
    _name in lazies ? AbstractSyntaxTree(o, ps) : evaluate(_name, ps)
end

_evaluation(o, ps::Vector{AbstractSyntaxTree}) = evaluation(o, ps)
_evaluation(o, ps::Vector{Bool}) =
    if o == ¬; !only(ps)
    elseif o == ∧; all(ps)
    elseif o == ∨; any(ps)
    else evaluate(o, ps)
    end
_evaluation(o, ps) = _evaluation(o, map(AbstractSyntaxTree, ps))

(o::Operator)(ps::AbstractSyntaxTree...) = evaluation(o, [ps...])
(o::Operator)(ps::Bool...) = _evaluation(o, [ps...])
(o::Operator)() = _evaluation(o, AbstractSyntaxTree[])
(o::Operator)(ps...) = o(map(AbstractSyntaxTree, ps)...)

print_expression(io, o, ps) =
    if o in [:tautology, :contradiction, :not]
        ns, ss = printings[o]

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

const p, q, r = @variables p q r

function ___register_operator(name, _arity)
    o = Operator{name}()

    if _arity == 2
        is_equivalent(o(p, q), o(q, p)) && push!(commutatives, name)
        is_equivalent(o(o(p, q), r), o(p, o(q, r))) && push!(associatives, name)
    end

    o
end

function register_binary(name, initial_value, associativity)
    o = Operator{name}()
    a, b, c, d = map(q -> is_equivalent(p, q), [o(⊤, p), o(p, ⊤), o(⊥, p), o(p, ⊥)])

    is_equivalent(o(p, q), o(q, p)) && push!(commutatives, name)
    is_equivalent(o(o(p, q), r), o(p, o(q, r))) && push!(associatives, name)

    if isnothing(initial_value)
        if a || b initial_values[name] = ⊤
        elseif c || d initial_values[name] = ⊥
        end
    else initial_values[name] = initial_value
    end

    if isnothing(associativity)
        _left, _right = a || c, b || d
        if _left && _right associativities[name] = operator_associativity(name) == :right ? right : left
        elseif _left associativities[name] = left
        elseif _right associativities[name] = right
        end
    else associativities[name] = associativity
    end
end

function register_printing(name, printing)
    current, ns, ss = firstindex(printing), Set{Int}(), SubstitutionString[]
    push!(lazies, name)

    for match in eachmatch(r"(\d+)", printing)
        capture, offset = only(match.captures), match.offset
        current < offset && push!(ss, printing[current:prevind(printing, offset)])
        push!(ss, capture[begin:end])
        push!(ns, length(ss))
        current = offset + ncodeunits(capture)
    end

    last = lastindex(printing)
    current ≤ last && push!(ss, printing[current:last])

    printings[name] = ns => ss
end

function _register_operator(name, arity, evaluation, initial_value, associativity, dual)
    arities[name] = arity
    arity == 2 && register_binary(name, initial_value, associativity)
end

function register_operator(name::Symbol, arity::Int, evaluation::AbstractSyntaxTree;
    initial_value::Union{Nothing, AbstractSyntaxTree} = nothing,
    associativity::Union{Nothing, Associativity} = nothing,
    printing::Union{Nothing, SubstitutionString} = nothing,
    dual::Union{Nothing, AbstractSyntaxTree} = nothing
)
    name in arities && error()
    evaluations[name] = normalize(∧, evaluation)
    isnothing(printing) || register_printing(name, printing)
    _register_operator(name, arity, evaluation, initial_value, associativity, dual)

    if isnothing(dual)
        _keys = keys(duals)

        for (_name, _evaluation) in evaluations
            if is_equivalent(evaluation, map(¬, ¬_evaluation))
                duals[name] = _name
                if !in(_name, _keys)
                    duals[_name] = name
                end
                break
            end
        end
    else duals[name] = dual
    end

    Operator{name}()
end

const lazies = Set{Symbol}()
const printings = Dict{Symbol, Pair{Set{Int}, Vector{SubstitutionString}}}()
const arities = Dict(map(((o, i),) -> name(o) => i, [⊤ => 0, ⊥ => 0, (¬) => 1, (∧) => 2, (∨) => 2]))
const associatives, commutatives = map(_ -> Set([:and, :or]), 1:2)
const initial_values = Dict(:and => AbstractSyntaxTree(⊤), :or => AbstractSyntaxTree(⊥))
const associativities = Dict(:and => left, :or => left)
const duals = Dict(append!([:identical => :identical, :not => :not], map((
    ⊤ => ⊥,
    (∧) => (∨),
    (↑) => (↓),
    (↔) => (↮),
    (→) => (↚),
    (←) => (↛)
)) do (o, _o)
    names = name(o) => name(_o)
    [names, reverse(names)]
end...))

for (name, printing) in map(((o, printing),) -> name(o) => printing, append!(
    map(o -> o => "$(symbol(o))", [⊤, ⊥]),
    Pair{Operator, String}[(¬) => "¬1"],
    map(o -> o => "1 $(symbol(o)) 2", [∧, ∨, →, ←, ↑, ↓, ↛, ↚, ↮, ↔])
))
    register_printing(name, printing)
end

const evaluations = Dict(Iterators.map(((o, p),) -> name(o) => p, @atomize [
    (𝒾) => $1,
    (→) => ¬$1 ∨ $2,
    (←) => $1 ∨ ¬$2,
    (↑) => ¬($1 ∧ $2),
    (↓) => ¬($1 ∨ $2),
    (↛) => $1 ∧ ¬$2,
    (↚) => ¬$1 ∧ $2,
    (↮) => ($1 ∨ $2) ∧ ¬($1 ∧ $2),
    (↔) => ($1 ∧ $2) ∨ ¬($1 ∨ $2)
]))

for (name, evaluation) in evaluations
    _register_operator(name, 1 + (name != :identical), evaluation, nothing, nothing, nothing)
end
