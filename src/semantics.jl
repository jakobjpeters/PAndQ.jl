
import Base: convert

using Combinatorics
using PrettyTables

"""
    p(valuation...)
    interpret(p::Proposition, valuation...)

Assign a [`Truth`](@ref) value to ```p```.

Let ```p``` be a [`Proposition`](@ref).
Let ```valuation``` be a function, callable object, dictionary, or any number of ```Pair```s
that map from [`atomic propositions`](@ref Atom) in ```p``` to their respective [`Truth`](@ref) values.

Calling ```p``` with an incomplete mapping will partially interpret ```p```.
This returns a ```Proposition``` of the ?*?same type as ```p```?*?
that is independent from every ```Atom```s in ```valuation```.

!!! warning
    If ```valuation``` does not return a ```Truth``` or errors, 

```

```
"""
interpret(p::Proposition, valuation) = _interpret(p, valuation)
interpret(p::Proposition, valuation::Dict) = interpret(p, q -> valuation[q])
interpret(p::Proposition, valuation::Pair...) = interpret(p, Dict(valuation))

_interpret(p::Truth, valuation...) = p
function _interpret(p::Atom, valuation)
    try
        return valuation(p)
    catch
        return p
    end
end
_interpret(p::Literal{Atom}, valuation) = _interpret(p.p, valuation)
_interpret(p::Literal{Tuple{typeof(not), Atom}}, valuation) = first(p.p)(_interpret(last(p.p), valuation))
_interpret(p::Tree, valuation) = first(p.p)(map(p -> _interpret(p, valuation), Base.tail(p.p))...)
_interpret(p::Union{Valuation, Normal}, valuation) = _interpret(Tree(p), valuation) # generic fallback

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
Base.:(==)(p::Truth, q::Truth) = p === q
Base.:(==)(p::Atom, q::Atom) = p === q
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
is_tautology(p::Proposition) = all(==(⊤), map(last, Valuation(p).p))

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
is_contradiction(p::Proposition) = is_tautology(¬p)

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
is_truth(p::Truth) = true
is_truth(p::Proposition) = unique(map(last, Valuation(p).p)) in [[⊤], [⊥]]

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

Returns a boolean on whether the given proposition is satisfiable
(not a [`contradiction`](@ref)).

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

Returns a boolean on whether the given proposition is falsifiable
(not a [`tautology`](@ref)).

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

"""
    is_Horn_clause(p::Proposition)

Returns a boolean on whether the given proposition is a
[Horn clause](https://en.wikipedia.org/wiki/Horn_clause).

```
julia>

```
"""
# is_Horn_clause(p::Clause)
# is_Horn_clause(p::Proposition) = false

"""
    is_definite_clause(p::Proposition)
"""
# is_definite_clause(p::Proposition) = false

"""
    neutral(operator)

Return the neutral element of the given operator.

```
julia>

```
"""
# neutral(::Union{map(typeof, [or, xor, not_if_then])...}) = ⊥
# neutral(::Union{map(typeof, [and, if_then, then_if])...}) = ⊤
# neutral(::Union{map(typeof, [not, nand, nor, not_then_if])...}) = nothing

# TODO: write all conversions

(p::Proposition)(valuation...) = interpret(p, valuation...)

not(::typeof(⊥)) = ⊤
not(::typeof(⊤)) = ⊥
not(p::Atom) = Literal((not, p))
not(p::Literal{Atom}) = not(p.p)
not(p::Literal{Tuple{typeof(not), Atom}}) = last(p.p) # double negation elimination
not(p::Tree{<:Tuple{typeof(not), Compound}}) = last(p.p) # double negation elimination
not(p::Tree) = Tree(not, p)
not(p::Valuation) = Valuation(map(interpretation -> first(interpretation) => not(last(interpretation)), p.p))
not(p::Clause{AO}) where AO <: Union{typeof(and), typeof(or)} = Clause(first(setdiff([and, or], [AO.instance])), map(not, p.p))
not(p::Normal{AO}) where AO <: Union{typeof(and), typeof(or)} = Normal(first(setdiff([and, or], [AO.instance])), map(not, p.p))
not(p::P) where P <: Proposition = P(not(Tree(p))) # generic fallback

and(::typeof(⊤), ::typeof(⊤)) = ⊤
and(::typeof(⊥), ::Truth) = ⊥ # domination law
and(::typeof(⊥), ::Proposition) = ⊥
and(::typeof(⊤), q::Truth) = q # identity law
and(::typeof(⊤), q::Proposition) = q
and(p::Proposition, q::Truth) = q ∧ p # commutative law
and(p::Union{Atom, Literal, Tree}, q::Union{Atom, Literal, Tree}) = Tree(and, p, q)
# and(p::Valuation, q::Valuation) = 
and(p::C, q::C) where C <: Clause{typeof(and)} = Clause(and, unique(append!(p.p, q.p)))
and(p::C, q::C) where C <: Clause{typeof(or)} = Normal(and, p.p, q.p)
# and(p::CNC, q::CNC) where CNC <: Union{Clause, Normal, Valuation} = CNC(p, q)
and(p::P, q::P) where P = P(and(Tree(p), Tree(q))) # generic fallback

Tree(::typeof(not), p::Atom) = Tree(not, Literal(p))
Tree(::typeof(not), p::Union{Literal, Tree}) = Tree((not, p))
Tree(::typeof(and), p::Atom, q::Atom) = Tree(and, Literal(p), Literal(q))
Tree(::typeof(and), p::Atom, q::Proposition) = Tree(and, Literal(p), q)
Tree(::typeof(and), p::Proposition, q::Atom) = Tree(and, p, Literal(q))
Tree(::typeof(and), p::Union{Literal, Tree}, q::Union{Literal, Tree}) = Tree((and, p, q))

Clause(::AO, ps::Union{Atom, Literal}...) where AO <: Union{typeof(and), typeof(or)} = Clause(AO.instance, collect(ps))
Normal(::AO, p::Clause...) where AO <: Union{typeof(and), typeof(or)} = Normal(AO.instance, collect(p))

Atom(p::Proposition) = convert(Atom, p)
Literal(p::Proposition) = convert(Literal, p)
Tree(p::Proposition) = convert(Tree, p)
Normal(::AO, p::Proposition) where AO <: Union{typeof(and), typeof(or)} = convert(Normal{AO}, p)
Valuation(p::Proposition) = convert(Valuation, p)

convert(::Type{Literal}, p::Atom) = Literal(p)
convert(::Type{Atom}, p::Literal{Atom}) = p.p
convert(::Type{Tree}, p::Normal{AO}) where AO <: Union{typeof(and), typeof(or)} = mapreduce(Tree, AO.instance, p.p)
convert(::Type{Tree}, p::typeof(⊤)) = not(Tree(⊥))
function convert(::Type{Tree}, p::typeof(⊥))
    p = Atom()
    return p ∧ ¬p
end
convert(::Type{Tree}, p::Union{Atom, Literal}) = p ∧ p
function convert(::Type{Tree}, p::Valuation)
    valid = filter(interpretation -> last(interpretation) == ⊤, p.p)

    isempty(valid) && return Tree(⊥)
    return Tree(
        mapreduce(
            interpretation -> mapreduce(
                pair -> last(pair) == ⊤ ? Literal(first(pair)) : not(first(pair)),
                and,
                first(interpretation)
            ),
            or,
            valid
        )
    )
end
convert(::Type{Tree}, p::Clause{typeof(and)}) = Tree(reduce(and, p.p, init = ⊤))
convert(::Type{Tree}, p::Clause{typeof(or)}) = Tree(reduce(or, p.p, init = ⊥))
convert(::Type{Valuation}, p::Truth) = Valuation(Tree(p))
function convert(::Type{Valuation}, p::Proposition)
    atoms = get_atoms(p)
    n = length(atoms)
    truth_sets = multiset_permutations([⊤, ⊥], [n, n], n)
    valuations = map(truth_set -> map(Pair{Atom, Truth}, atoms, truth_set), truth_sets)
    truths = map(valuation -> interpret(p, Dict(valuation)), valuations)

    return Valuation(map(Pair, valuations, truths))
end
convert(::Type{Clause{typeof(and)}}, p::typeof(⊤)) = Clause(and)
convert(::Type{Clause{typeof(or)}}, p::typeof(⊥)) = Clause(or)
convert(::Type{Normal{typeof(and)}}, p::Proposition) = not(Normal(or, ¬p))
function convert(::Type{Normal{typeof(or)}}, p::Proposition)
    q = Valuation(p)
    # TODO: change `===` to `==` - fixes `Normal(and, ⊥)`
    interpretations =
        if is_tautology(q)
            atom = Atom()
            [[atom => ⊤], [atom => ⊥]]
        elseif is_contradiction(q)
            atom = Atom()
            [[atom => ⊤, atom => ⊥]]
        else
            map(
                first,
                filter(
                    literal -> last(literal) == ⊤,
                    q.p
                )
            )
        end

    clauses = map(
        interpretation -> Clause(
            and,
            map(
                pair -> last(pair) == ⊤ ? Literal(first(pair)) : not(first(pair)),
                interpretation
            )
        ),
        interpretations
    )

    return Normal(or, clauses)
end
# function convert(::Type{E}, p::Proposition) where E <: Expressive
#     p isa E && return p
#     return E(Tree(p))
# end
convert(::Type{P}, p::P) where P <: Proposition = p # generic fallback


import Base.promote_rule
promote_rule(p::Atom, q::Literal) = Literal
promote_rule(p::P, q::P) where P <: Type{Proposition} = Valuation
