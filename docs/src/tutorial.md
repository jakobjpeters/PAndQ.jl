
```@meta
DocTestSetup = :(using PAndQ)
```

# Tutorial

## Truth Values

A truth value is logic's version of a boolean `true` or `false`. These concepts can also be represented as a `1` or a `0`. Similarly, we use [`tautology`](@ref) and [`contradiction`](@ref). These are commonly represented with the symbols `⊤` and `⊥`. These truth values have additional meaning, which will be discussed further on.

## [Operators](@id tutorial_operators)

Just like with boolean values, we can perform operations on truth values. Both `!` and the symbol `¬` represent the unary operator `not`.

```jldoctest
julia> !true == false # boolean negation
true

julia> ¬⊤ == ⊥ # logical negation
true
```

There are several operators that accept multiple arguments. Both `&&` and the symbol `∧` represent the binary operator [`and`](@ref). Both `||` and the symbol `∨` represent the binary operator [`or`](@ref).

```jldoctest
julia> true && false == false # boolean and
true

julia> true || false == true # boolean or
true

julia> ⊤ ∧ ⊥ == ⊥ # logical and
true

julia> ⊤ ∨ ⊥ == ⊤ # logical or
true
```

There are several more operators, which will be discussed later. Use the [`arity`](@ref) function to determine the number of arguments for an operator.

```jldoctest
julia> arity(contradiction)
0

julia> arity(not)
1

julia> arity(and)
2
```

## Propositions

A [`Proposition`](@ref) is a statement that can be either true or false. For example, "Logic is fun" is a proposition because it may be true for you but false for someone else. Note that the proposition exists on its own, regardless of whether or not it is known to be true or false.

```jldoctest 1
julia> p = Atom("Logic is fun")
Atom("Logic is fun")

julia> q = Atom("Julia is awesome")
Atom("Julia is awesome")
```

We can also perform operations on propositions. In a written form, we can negate the above proposition by saying "Logic is not fun". We could combine two propositions using another operator, such as "Logic is fun and Julia is awesome".

```jldoctest 1
julia> r = ¬p
¬Atom("Logic is fun")

julia> s = p ∧ q
Atom("Logic is fun") ∧ Atom("Julia is awesome")
```

An [`Atom`](@ref)ic proposition is such that it has not been operated on and is not composed of any other propositions. Thus the first proposition, "Logic is fun", is atomic. A [`Compound`](@ref) proposition is any proposition that is not an atom. "Logic is not fun" and "Logic is fun and Julia is awesome" are [`Compound`](@ref) propositions. A [`Literal`](@ref) proposition is a proposition that is either an atom or its negation. "Logic is fun" and "Logic is not fun" are literals. Since propositions can be nested arbitrarily, a [`Tree`](@ref) structure can be used to represent them.

```jldoctest 1
julia> p isa Atom && q isa Atom && Atom <: Proposition
true

julia> r isa Compound && s isa Compound && Compound <: Proposition
true

julia> r isa Literal && Literal <: Compound
true

julia> s isa Tree && Tree <: Compound
true
```

In mathematics, it's useful to replace individual numbers with a symbolic variable that can represent any number. Since these propositions are for demonstration and could really be any proposition, we will do the same with our propositions `p` and `q`. To do so, we will use the [`@atoms`](@ref) macro to define each atomic proposition as a `const`ant. Alternatively, you can construct this kind of atom with a `Symbol`: `Atom(:p)`.

```jldoctest 2
julia> @atoms p q
2-element Vector{Atom{Symbol}}:
 p
 q

julia> p
p

julia> q
q

julia> r = ¬p
¬p

julia> s = p ∧ q
p ∧ q
```

The function [`atoms`](@ref) returns an iterator of each `Atom` in a proposition.

```jldoctest 2
julia> collect(atoms(r))
1-element Vector{Atom{Symbol}}:
 p

julia> collect(atoms(s))
2-element Vector{Atom{Symbol}}:
 p
 q
```

## Semantics

We know that since these are propositions, they can be true or false. If you think that "Logic is fun", it would be invalid to assign it the valuation false. So the proposition `p` is true if you think that "Logic is fun" and is false otherwise. If we assign the value true to the proposition "Logic is fun", then we know that the validity of the proposition "Logic is fun and Julia is awesome" depends on whether or not "Julia is awesome". If "Julia is awesome" is assigned false, then the conjunction of the two propositions is false. Use the [`interpret`](@ref) function to assign meaning to atomic propositions and then simplify the proposition.

```jldoctest 2
julia> interpret(a -> ⊥, r)
tautology (generic function with 1 method)

julia> s(p => ⊤, q => ⊥)
contradiction (generic function with 1 method)
```

Assigning meaning to any number of atomic propositions is called a [`Valuation`](@ref valuations). Since `p` can only be true or false, it has two possible valuations. `r` doesn't depend on any other propositions, because it is just a negation of `p`. Thus, it also has two possible valuations. However, the valuation and result of evaluating it are not longer the same. Since `r == ¬p`, if `p` is assigned true, then `r` is determined to be false, and vice versa. An [`interpretation`](@ref interpretations) is the truth values of any number of propositions determined by a given valuation.

```jldoctest 2
julia> collect(valuations(r))
2-element Vector{Vector}:
 Pair{Atom{Symbol}, typeof(tautology)}[Atom(:p) => PAndQ.tautology]
 Pair{Atom{Symbol}, typeof(contradiction)}[Atom(:p) => PAndQ.contradiction]

julia> collect(interpretations(r))
2-element Vector{Function}:
 contradiction (generic function with 1 method)
 tautology (generic function with 1 method)
```

Since `s` contains two atomic propositions, there are four valuations: `p` is true and `q` is true, `p` is false and `q` is true, `p` is true and `q` is false, and `p` is false and `q` is false. Each additional atomic proposition in a proposition doubles the number of possible valuations. Mathematically, there are `2 ^ n` valuations where `n = length(unique!(collect(atoms(p))))`. Since each interpretation depends on a valuation, the number of valuations and interpretations are equal.

```jldoctest 2
julia> n = length(unique!(collect(atoms(s))))
2

julia> length(valuations(s)) == length(interpretations(s)) == 2 ^ n == 4
true
```

We are often interested in valuations that result in a valid interpretation. This is accomplished with the [`solve`](@ref) function. The proposition `s` is the conjunction of `p` and `p`, so it is only true if both `p` and `q` are true. Each of the other three possible valuations are invalid.

```jldoctest 2
julia> collect(solve(s))
1-element Vector{Vector{Pair{Atom{Symbol}, typeof(tautology)}}}:
 [Atom(:p) => PAndQ.tautology, Atom(:q) => PAndQ.tautology]

julia> collect(solve(¬s))
3-element Vector{Vector}:
 Pair{Atom{Symbol}}[Atom(:p) => PAndQ.contradiction, Atom(:q) => PAndQ.tautology]
 Pair{Atom{Symbol}}[Atom(:p) => PAndQ.tautology, Atom(:q) => PAndQ.contradiction]
 Pair{Atom{Symbol}, typeof(contradiction)}[Atom(:p) => PAndQ.contradiction, Atom(:q) => PAndQ.contradiction]
```

A proposition [`is_satisfiable`](@ref) if there is at least one valid interpretation. A proposition [`is_falsifiable`](@ref) if there is at least one invalid interpretation. A proposition [`is_contingency`](@ref) if it is both satisfiable and falsifiable.

```jldoctest 2
julia> is_satisfiable(s) && is_falsifiable(s) && is_contingency(s)
true
```

A proposition is a tautology if every possible interpretation is true. A proposition is a contradiction if every possible interpretation is false. For example, `p ∧ ¬p` is always interpreted as false because either `p` or `¬p` must be false. `p ∨ ¬p` is always interpreted as true because either `p` or `¬p` must be true. Use the functions [`is_tautology`](@ref), [`is_contradiction`](@ref), and [`is_truth`](@ref) to check whether a proposition is logically equivalent to a truth value.

```jldoctest 2
julia> t = p ∧ ¬p
p ∧ ¬p

julia> u = p ∨ ¬p
p ∨ ¬p

julia> collect(interpretations(t))
2-element Vector{typeof(contradiction)}:
 contradiction (generic function with 1 method)
 contradiction (generic function with 1 method)

julia> collect(interpretations(u))
2-element Vector{typeof(tautology)}:
 tautology (generic function with 1 method)
 tautology (generic function with 1 method)

julia> is_contradiction(t) && is_tautology(u)
true

julia> is_truth(t) && is_truth(u)
true
```

Two propositions are logically equivalent if their interpretation is equivalent for every possible valuation. For example, the propositions `¬(¬p ∧ ¬q)` and `p ∨ q` are logically equivalent. In fact, the `or` operator is implemented this way. Use [`==`](@ref) to test that two propositions are logically equivalent. Use `===` to test that two propositions have an identical internal representation.

```jldoctest 2
julia> ¬(¬p ∧ ¬q) == p ∨ q
true

julia> ¬(¬p ∧ ¬q) === p ∨ q
false
```

## Visualization

It would also be helpful to enumerate each valuation and interpretation in a visual format. This is accomplished by creating a [`TruthTable`](@ref). A truth table is a table where each column in the header identifies a proposition, and each row contains an interpretation (including the valuation of atomic propositions). To demonstrate these, we will use the [`xor`](@ref) operator, represented by the symbol `⊻`. Try to understand the meaning of this operator as it is interpreted with different valuations.

```jldoctest 2
julia> TruthTable(p ⊻ q)
┌──────┬──────┬───────┐
│ p    │ q    │ p ⊻ q │
│ Atom │ Atom │ Tree  │
├──────┼──────┼───────┤
│ ⊤    │ ⊤    │ ⊥     │
│ ⊥    │ ⊤    │ ⊤     │
├──────┼──────┼───────┤
│ ⊤    │ ⊥    │ ⊤     │
│ ⊥    │ ⊥    │ ⊥     │
└──────┴──────┴───────┘
```
