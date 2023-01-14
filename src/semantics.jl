
using Combinatorics
using PrettyTables

(::Not)(::typeof(⊥)) = ⊤
(::Not)(::typeof(⊤)) = ⊥
(::Not)(p::Atom) = Literal((Not(), p))
(::Not)(p::Literal{Atom}) = not(p.p)
(::Not)(p::Literal{Tuple{Not, Atom}}) = last(p.p)
(::Not)(p::Compound) = Tree(Not(), p)
(::Not)(p::Tree{<:Tuple{Not, Compound}}) = last(p.node) # double negation elimination
function (::Not)(p::Normal{B}) where B <: Union{And, Or}
    clauses = map(clause -> map(not, clause), p.clauses)
    b = B == And ? Or : And

    return Normal{b}(clauses)
end

(::And)(::typeof(⊤), ::typeof(⊤)) = ⊤
(::And)(::typeof(⊥), ::Truth) = ⊥ # domination law
(::And)(::typeof(⊥), ::Proposition) = ⊥
(::And)(::typeof(⊤), q::Truth) = q # identity law
(::And)(::typeof(⊤), q::Proposition) = q
(::And)(p::Proposition, q::Truth) = q ∧ p # commutative law
(::And)(p::Proposition, q::Proposition) = Tree(And(), p, q)

(p::Union{Truth, Contingency})() = p
(p::Normal)() = Tree(p)()

# ToDo: make type stable
function (p::Proposition)()
    atoms = get_atoms(p)
    n = length(atoms)
    truth_sets = multiset_permutations([⊤, ⊥], [n, n], n)
    valuations = map(truth_set -> map(Pair{Atom, Truth}, atoms, truth_set), truth_sets)
    truths = map(valuation -> interpret(p -> Dict(valuation)[p], p), valuations)

    union(truths) == [⊤] && return ⊤
    union(truths) == [⊥] && return ⊥
    return Contingency(map(Pair, valuations, truths))
end

"""
    interpret(valuation, p::Proposition)

Given a valuation function that maps from the [`atomic propositions`](@ref Atom)
in ```p``` to their respective [`Truth`](@ref) values,
assign a truth value to ```p```.

See also [`Proposition`](@ref).
"""
interpret(valuation, p::Proposition) = p(Dict(map(p -> p => valuation(p), get_atoms(p))))

(p::Atom)(interpretations) = interpretations[p]
(p::Literal{Atom})(interpretations) = p.p(interpretations)
(p::Literal{Tuple{Not, Atom}})(interpretations) = first(p.p)(last(p.p)(interpretations))
(p::Tree)(interpretations) = first(p.node)(map(p -> p(interpretations), Base.tail(p.node))...)
(p::Normal)(interpretations) = Tree(p)(interpretations)

"""
    p == q
    ==(p::Proposition, q::Proposition)
    isequal(p::Proposition, q::Proposition)

Returns a boolean indicating whether ```p``` and ```q``` are logically equivalent.

See also [`Proposition`](@ref).

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
Base.:(==)(p::TP, q::TP) where TP <: Union{Truth, Atom} = p === q
Base.:(==)(p::Proposition, q::Proposition) = is_tautology(p ↔ q)

"""
    is_tautology(p::Proposition)

Returns a boolean on whether the given proposition is a [`tautology`](@ref).

This function is equivalent to ```p == ⊤```.

See also [`Proposition`](@ref) and [`==`](@ref).

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
is_tautology(p::Proposition) = _is_tautology(p())

_is_tautology(::typeof(⊤)) = true
_is_tautology(::Any) = false

"""
    is_contradiction(p::Proposition)

Returns a boolean on whether the given proposition is a [`contradiction`](@ref).

This function is equivalent to ```p == ⊥```.

See also [`Proposition`](@ref) and [`==`](@ref).

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
is_contradiction(p::Proposition) = p == ⊥

"""
    is_truth(p::Proposition)

Returns a boolean on whether the given proposition is a [`Truth`](@ref)
(either a [`tautology`](@ref) or [`contradiction`](@ref)).

See also [`Proposition`](@ref).

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
is_truth(p::Proposition) = _is_truth(p())

_is_truth(p::Truth) = true
_is_truth(p) = false

"""
    is_contingency(p::Proposition)

Returns a boolean on whether the given proposition is a contingency
(neither a [`tautology`](@ref) or [`contradiction`](@ref)).

See also [`Proposition`](@ref).

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
is_contingency(p::Proposition) = !is_truth(p)

"""
    is_satisfiable(p::Proposition)

Returns a boolean on whether the given proposition is satisfiable (not a [`contradiction`](@ref)).

See also [`Proposition`](@ref).

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
is_satisfiable(p::Proposition) = !is_contradiction(p)

"""
    is_falsifiable(p::Proposition)

Returns a boolean on whether the given proposition is falsifiable (not a [`is_tautology`](@ref)).

See also [`Proposition`](@ref).

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
is_falsifiable(p::Proposition) = !is_tautology(p)
