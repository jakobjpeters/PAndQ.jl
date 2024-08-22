
import Base: Bool, convert, promote_rule
using Base.Iterators: flatten, product, repeated
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
valuations(p::Union{Operator, AbstractSyntaxTree}) = valuations(collect(atoms(p)))

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
is_tautology(o::Operator) =
    if o == ⊤ true
    elseif o == ⊥ false
    else error()
    end
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
is_equivalent(p::Operator, q::Operator) = p in [⊤, ⊥] ? p == q : error()
is_equivalent(p::AbstractSyntaxTree, q::Operator) =
    if q == ⊤ is_tautology(p)
    elseif q == ⊥ is_contradiction(p)
    else error()
    end
is_equivalent(p::Operator, q::AbstractSyntaxTree) = is_equivalent(q, p)
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
Bool(o::Operator) = convert(Bool, o)

# Constructors

AbstractSyntaxTree(p) = convert(AbstractSyntaxTree, p)

# Utilities

convert(::Type{Bool}, o::Operator) =
    if o == ⊤ true
    elseif o == ⊥ false
    else error()
    end

"""
    convert(::Type{<:AbstractSytnaxTree}, p)

See also [`AbstractSyntaxTree`](@ref).
"""
convert(::Type{AbstractSyntaxTree}, p::Operator) = AbstractSyntaxTree(operator, p)
convert(::Type{AbstractSyntaxTree}, p::Symbol) = AbstractSyntaxTree(variable, p)
convert(::Type{AbstractSyntaxTree}, p::Some) = AbstractSyntaxTree(constant, p)

"""
    promote_rule
"""
promote_rule(::Type{Bool}, ::Type{Operator}) = Bool
promote_rule(::Type{Operator}, ::Type{AbstractSyntaxTree}) = AbstractSyntaxTree

# Interface Implementation

arity(o) = get(arities, o, nothing)

initial_value(o) = o in keys(initial_values) ? initial_values[o] : nothing

const symbols = Dict(
    ⊤ => :⊤, ⊥ => :⊥,
    𝒾 => :𝒾, (¬) => :¬,
    (∧) => :∧, (↑) => :↑, (↓) => :↓, (∨) => :∨, (↮) => :↮, (↔) => :↔, (→) => :→, (↛) => :↛, (←) => :←, (↚) => :↚,
    (⋀) => :⋀, (⋁) => :⋁
)

symbol(o) = symbols[o]

dual(o) = get(duals, o) do
    _arity = arity(o)
    register_operator(gensym("dual_$(o.name)"), _arity, map(¬, ¬AbstractSyntaxTree(
        operator, o, map(i -> @atomize($i), 1:_arity))))
end

# inverse, contrapositive, converse

is_commutative(o) = o in commutatives

is_associative(o) = o in associatives

Base.Bool(p::AbstractSyntaxTree) = p.kind == operator ? Bool(p.value::Operator) : error()

evaluate(o, ps) =
    if o in [⊤, ⊥] AbstractSyntaxTree(operator, o)
    elseif o == ¬
        q = only(ps)
        branches, o_q = q.branches, nodevalue(q)
        o_q == (¬) ? only(branches) : evaluate(dual(o_q), map(¬, branches))
    elseif o in [∧, ∨]
        q, r = ps
        _initial_value, o_q = initial_value(o), nodevalue(q)
        if o_q == _initial_value r
        else
            o_r = nodevalue(r)
            if o_r == _initial_value q
            else
                dual_initial_value = dual(_initial_value)
                any(_o -> _o in AbstractSyntaxTree[⊤, ⊥], AbstractSyntaxTree[o_q, o_r]) ?
                    AbstractSyntaxTree(operator, dual_initial_value) :
                    AbstractSyntaxTree(operator, o, [q, r])
            end
        end
    elseif o == ⋀ fold(𝒾, (∧) => ps)
    elseif o == ⋁ fold(𝒾, (∨) => ps)
    else o in keys(evaluations) ?
        interpret(map(((i, p),) -> @atomize($i => p), enumerate(ps)), evaluations[o]) :
        error()
    end

evaluation(o, ps) = o in lazies ? AbstractSyntaxTree(o, ps) : evaluate(o, ps)

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
    if o in [⊤, ⊥, ¬]
        ns, ss = printings[o]

        for (i, s) in enumerate(ss)
            if i in ns
                print_proposition(io, ps[parse(Int, s)])
            else print(io, s)
            end
        end
    elseif o in [∧, ∨, ↑, ↓, →, ↛, ←, ↚, ↔, ↮]
        qs, stack = AbstractSyntaxTree[], AbstractSyntaxTree[]

        if is_associative(o)
            append!(stack, ps)

            while !isempty(stack)
                q = pop!(stack)
                nodevalue(q) == o ? append!(stack, (children(q))) : push!(qs, q)
            end

            reverse!(qs)
        else append!(qs, ps)
        end

        _show(print_proposition, io, qs) do io
            print(io, " ")
            show(io, MIME"text/plain"(), o)
            print(io, " ")
        end
    else print_expression(io, o, ps)
    end

_print_proposition(io, p::AbstractSyntaxTree) =
    if p.kind == variable print(io, p.value::Symbol)
    elseif p.kind == constant
        print(io, "\$(")
        show(io, something(p.value))
        print(io, ")")
    else print_expression(io, nodevalue(p), children(p))
    end

print_proposition(io, p) = _print_proposition(IOContext(io, :root => false), p)

const p, q, r = @variables p q r

function register_binary(o, initial_value, associativity)
    a, b, c, d = map(q -> is_equivalent(p, q), [o(⊤, p), o(p, ⊤), o(⊥, p), o(p, ⊥)])

    is_equivalent(o(p, q), o(q, p)) && push!(commutatives, o)
    is_equivalent(o(o(p, q), r), o(p, o(q, r))) && push!(associatives, o)

    if isnothing(initial_value)
        if a || b initial_values[o] = ⊤
        elseif c || d initial_values[o] = ⊥
        end
    else initial_values[o] = initial_value
    end

    if isnothing(associativity)
        if a || c associativities[o] = left
        elseif b || d associativities[o] = right
        end
    else associativities[o] = associativity
    end
end

function register_printing(o, printing)
    current, ns, ss = firstindex(printing), Set{Int}(), SubstitutionString[]
    push!(lazies, o)

    for match in eachmatch(r"(\d+)", printing)
        capture, offset = only(match.captures), match.offset
        current < offset && push!(ss, printing[current:prevind(printing, offset)])
        push!(ss, capture[begin:end])
        push!(ns, length(ss))
        current = offset + ncodeunits(capture)
    end

    last = lastindex(printing)
    current ≤ last && push!(ss, printing[current:last])

    printings[o] = ns => ss
end

function _register_operator(o, arity, initial_value, associativity)
    arities[o] = arity
    arity == 2 && register_binary(o, initial_value, associativity)
end

function register_operator(o::Operator, arity::Int, evaluation::AbstractSyntaxTree;
    initial_value::Union{Nothing, AbstractSyntaxTree} = nothing,
    associativity::Union{Nothing, Associativity} = nothing,
    printing::Union{Nothing, SubstitutionString} = nothing,
    dual::Union{Nothing, AbstractSyntaxTree} = nothing
)
    o in arities && error()
    evaluations[o] = normalize(∧, evaluation)
    isnothing(printing) || register_printing(o, printing)
    _register_operator(o, arity, initial_value, associativity)

    if isnothing(dual)
        _keys = keys(duals)

        for (_o, _evaluation) in evaluations
            if is_equivalent(evaluation, map(¬, ¬_evaluation))
                duals[o] = _o
                if !in(_o, _keys)
                    duals[_o] = o
                end
                break
            end
        end
    else duals[o] = dual
    end

    o
end

const lazies = Set{Operator}()
const printings = Dict{Operator, Pair{Set{Int}, Vector{SubstitutionString}}}()
const arities = Dict(⊤ => 0, ⊥ => 0, (¬) => 1, (∧) => 2, (∨) => 2)
const associatives, commutatives = map(_ -> Set([∧, ∨]), 1:2)
const initial_values = Dict((∧) => ⊤, (∨) => ⊥)
const associativities = Dict((∧) => left, (∨) => left)
const duals = Dict(append!([𝒾 => 𝒾, (¬) => (¬)], map(pair -> [pair, reverse(pair)], (
    ⊤ => ⊥,
    (∧) => (∨),
    (↑) => (↓),
    (↔) => (↮),
    (→) => (↚),
    (←) => (↛)
))...))

for (o, printing) in flatten([
    map(o -> o => "$(symbol(o))", [⊤, ⊥]),
    Pair{Operator, String}[(¬) => "¬1"],
    map(o -> o => "1 $(symbol(o)) 2", [∧, ∨, →, ←, ↑, ↓, ↛, ↚, ↮, ↔])
])
    register_printing(o, printing)
end

const evaluations = @atomize Dict(
    (𝒾) => $1,
    (→) => ¬$1 ∨ $2,
    (←) => $1 ∨ ¬$2,
    (↑) => ¬($1 ∧ $2),
    (↓) => ¬($1 ∨ $2),
    (↛) => $1 ∧ ¬$2,
    (↚) => ¬$1 ∧ $2,
    (↮) => ($1 ∨ $2) ∧ ¬($1 ∧ $2),
    (↔) => ($1 ∧ $2) ∨ ¬($1 ∨ $2)
)

for (o, evaluation) in evaluations
    _register_operator(o, 1 + (o != 𝒾), nothing, nothing)
end
