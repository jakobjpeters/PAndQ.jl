
using Combinatorics
using PrettyTables

(::Not)(::typeof(⊥)) = ⊤
(::Not)(::typeof(⊤)) = ⊥
(::Not)(p::Primitive) = Literal((Not(), p))
(::Not)(p::Literal{Primitive}) = not(p.ϕ)
(::Not)(p::Literal{Tuple{Not, Primitive}}) = last(p.ϕ)
(::Not)(p::Compound) = Tree(Not(), p)
(::Not)(p::Tree{<:Tuple{Not, Compound}}) = last(p.ϕ) # double negation elimination
function (::Not)(p::Normal{B}) where B <: Union{And, Or}
    clauses = map(clause -> map(not, clause), p.clauses)
    b = B == And ? Or : And

    return Normal{b}(clauses)
end

(::And)(::typeof(⊤), ::typeof(⊤)) = ⊤
(::And)(::typeof(⊥), ::Truth) = ⊥ # domination law
(::And)(::typeof(⊥), ::Language) = ⊥
(::And)(::typeof(⊤), q::Truth) = q # identity law
(::And)(::typeof(⊤), q::Language) = q
(::And)(p::Language, q::Truth) = q ∧ p # commutative law
(::And)(p::Language, q::Language) = Tree(And(), p, q)

(p::Union{Truth, Contingency})() = p
(p::Normal)() = Tree(p)()

# ToDo: make type stable
function (p::Language)()
    primitives = get_primitives(p)
    n = length(primitives)
    truth_sets = multiset_permutations([⊤, ⊥], [n, n], n)
    valuations = map(truth_set -> map(Pair{Primitive, Truth}, primitives, truth_set), truth_sets)
    truths = map(valuation -> interpret(p -> Dict(valuation)[p], p), valuations)

    union(truths) == [⊤] && return ⊤
    union(truths) == [⊥] && return ⊥
    return Contingency(map(Pair, valuations, truths))
end

"""
    interpret(valuation, p::Language)

Given a valuation function that maps from the [`Primitive`](@ref)
propositions in ```p``` to their respective [`Truth`](@ref) values,
assign a truth value to ```p```.

See also [`Language`](@ref).
"""
interpret(valuation, p::Language) = p(Dict(map(p -> p => valuation(p), get_primitives(p))))

(p::Primitive)(interpretations) = interpretations[p]
(p::Literal{Primitive})(interpretations) = p.ϕ(interpretations)
(p::Literal{Tuple{Not, Primitive}})(interpretations) = first(p.ϕ)(last(p.ϕ)(interpretations))
(p::Tree)(interpretations) = first(p.ϕ)(map(ϕ -> ϕ(interpretations), Base.tail(p.ϕ))...)
(p::Normal)(interpretations) = Tree(p)(interpretations)

"""
    p == q
    ==(p::Language, q::Language)
    isequal(p::Language, q::Language)

Returns a boolean indicating whether ```p``` and ```q``` are logically equivalent.

See also [`Language`](@ref).

!!! info
    The ```≡``` symbol is sometimes used to represent logical equivalence.
    However, Julia uses ```≡``` as an alias for the builtin function ```===```
    which cannot have methods added to it.
    Use this function to compare identity rather than equivalence.

# Examples
```
julia> p == ¬p
false

julia> (p → q) ∧ (p ← q) == ¬(p ⊻ q)
true

julia> (p → q) ∧ (p ← q) === ¬(p ⊻ q)
false
```
"""
Base.:(==)(p::TP, q::TP) where TP <: Union{Truth, Primitive} = p === q
Base.:(==)(p::Language, q::Language) = is_tautology(p ↔ q)

"""
    is_tautology(p::Language)

Returns a boolean on whether the given proposition is a [`tautology`](@ref).

This function is equivalent to ```p == ⊤```.

See also [`Language`](@ref) and [`==`](@ref).

# Examples
```jldoctest
julia> is_tautology(⊤)
true

julia> is_tautology(p)
false

julia> is_tautology(¬(p ∧ ¬p))
true
```
"""
is_tautology(p::Language) = _is_tautology(p())

_is_tautology(::typeof(⊤)) = true
_is_tautology(::Any) = false

"""
    is_contradiction(p::Language)

Returns a boolean on whether the given proposition is a [`contradiction`](@ref).

This function is equivalent to ```p == ⊥```.

See also [`Language`](@ref) and [`==`](@ref).

# Examples
```jldoctest
julia> is_contradiction(⊥)
true

julia> is_contradiction(p)
false

julia> is_contradiction(p ∧ ¬p)
true
```
"""
is_contradiction(p::Language) = p == ⊥

"""
    is_truth(p::Language)

Returns a boolean on whether the given proposition is a [`Truth`](@ref)
(either a [`tautology`](@ref) or [`contradiction`](@ref)).

See also [`Language`](@ref).

# Examples
```jldoctest
julia> is_truth(⊤)
true

julia> is_truth(p ∧ ¬p)
true

julia> is_truth(p)
false

julia> is_truth(p ∧ q)
false
```
"""
is_truth(p::Language) = _is_truth(p())

_is_truth(p::Truth) = true
_is_truth(p) = false

"""
    is_contingency(p::Language)

Returns a boolean on whether the given proposition is a contingency
(neither a [`tautology`](@ref) or [`contradiction`](@ref)).

See also [`Language`](@ref).

# Examples
```jldoctest
julia> is_contingency(⊤)
false

julia> is_contingency(p ∧ ¬p)
false

julia> is_contingency(p)
true

julia> is_contingency(p ∧ q)
true
```
"""
is_contingency(p::Language) = !is_truth(p)

"""
    is_satisfiable(p::Language)

Returns a boolean on whether the given proposition is satisfiable (not a [`contradiction`](@ref)).

See also [`Language`](@ref).

# Examples
```jldoctest
julia> is_satisfiable(⊤)
true

julia> is_satisfiable(p ∧ ¬p)
false

julia> is_satisfiable(p)
true

julia> is_satisfiable(p ∧ q)
true
```
"""
is_satisfiable(p::Language) = !is_contradiction(p)

"""
    is_falsifiable(p::Language)

Returns a boolean on whether the given proposition is falsifiable (not a [`is_tautology`](@ref)).

See also [`Language`](@ref).

# Examples
```jldoctest
julia> is_falsifiable(⊥)
true

julia> is_falsifiable(¬(p ∧ ¬p))
false

julia> is_falsifiable(p)
true

julia> is_falsifiable(p ∧ q)
true
```
"""
is_falsifiable(p::Language) = !is_tautology(p)
