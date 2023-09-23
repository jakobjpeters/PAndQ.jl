
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

A [`Proposition`](@ref) is a statement that can be either true or false. For example, "Logic is fun" is a proposition because it may be true for you but false for someone else. This proposition has a known value, so it is a [`Constant`](@ref). Note that the proposition exists independently of whether it is known to be true or false.

```jldoctest 1
julia> p = Constant("Logic is fun")
$("Logic is fun")

julia> q = Constant("Julia is awesome")
$("Julia is awesome")
```

We can also perform operations on propositions. In a written form, we can negate the above proposition by saying "Logic is not fun". We could combine two propositions using another operator, such as "Logic is fun and Julia is awesome".

```jldoctest 1
julia> ¬p
¬$("Logic is fun")

julia> p ∧ q
$("Logic is fun") ∧ $("Julia is awesome")
```

In mathematics, it's useful to replace individual numbers with a symbolic [`Variable`](@ref) that can represent an unknown value. Since these atoms are for demonstration and represent an unknown value, we will do the same with our propositions `p` and `q`.

```jldoctest 1
julia> p = Variable(:p)
p

julia> q = Variable(:q)
q

julia> ¬p
¬p

julia> p ∧ q
p ∧ q
```

Propositions that do not contain any structure are called [`Atom`](@ref)ic. `Constant`s and `Variable`s are [`Atom`](@ref)ic propositions. [`Compound`](@ref) propositions are formed by connecting atomic propositions with logical operators. A [`Literal`](@ref) is a proposition that is either an atom or its negation. Since propositions can be nested arbitrarily, a [`Tree`](@ref) structure can be used to represent them.

```jldoctest 1
julia> p isa Atom
true

julia> ¬p isa Literal && ¬p isa Compound
true

julia> p ∧ q isa Tree && p ∧ q isa Compound
true
```

The function [`atoms`](@ref) returns an iterator of each `Atom` in a proposition.

```jldoctest 1
julia> collect(atoms(¬p))
1-element Vector{Variable}:
 p

julia> collect(atoms(p ∧ q))
2-element Vector{Variable}:
 p
 q
```

## Semantics

We know that since these are propositions, they can be true or false. If you think that "Logic is fun", it would be invalid to assign it the valuation false. So the proposition `p` is true if you think that "Logic is fun" and is false otherwise. If we assign the value true to the proposition "Logic is fun", then we know that the validity of the proposition "Logic is fun and Julia is awesome" depends on whether or not "Julia is awesome". If "Julia is awesome" is assigned false, then the conjunction of the two propositions is false. Use the [`interpret`](@ref) function to assign meaning to atomic propositions and then simplify the proposition.

```jldoctest 1
julia> interpret(a -> false, ¬p)
true

julia> (p ∧ q)(p => true, q => false)
false
```

Assigning meaning to any number of atomic propositions is called a [`valuation`](@ref valuations). Since `p` can only be true or false, those are it's possible valuations. An [`interpretation`](@ref interpretations) is the truth value of propositions that is determined by a given valuation. Since `p` is atomic, its valuation and interpretation are the same. `¬p` doesn't depend on any other propositions, so it also has two possible valuations. However, the valuation and the interpretation are no longer the same. If `p` is assigned true, then `¬p` is determined to be false, and vice versa.

```jldoctest 1
julia> collect(valuations(¬p))
2-element Vector{Vector{Pair{Variable, Bool}}}:
 [Variable(:p) => 1]
 [Variable(:p) => 0]

julia> collect(interpretations(¬p))
2-element Vector{Bool}:
 0
 1
```

Since `p ∧ q` contains two atomic propositions, there are four valuations: `p` is true and `q` is true, `p` is false and `q` is true, `p` is true and `q` is false, and `p` is false and `q` is false. Each additional atomic proposition in a proposition doubles the number of possible valuations. Mathematically, there are `2 ^ n` valuations where `n = length(unique!(collect(atoms(p))))`. Since each interpretation depends on a valuation, the number of valuations and interpretations are equal.

```jldoctest 1
julia> n = length(unique!(collect(atoms(p ∧ q))))
2

julia> length(valuations(p ∧ q)) == length(interpretations(p ∧ q)) == 2 ^ n == 4
true
```

It is useful to find valuations that determine valid interpretations. This is accomplished with the [`solve`](@ref) function. The proposition `p ∧ q` is determined to be true with the valuation that both `p` and `q` are true. Each of the other three possible valuations are invalid.

```jldoctest 1
julia> collect(solve(p ∧ q))
1-element Vector{Vector{Pair{Variable, Bool}}}:
 [Variable(:p) => 1, Variable(:q) => 1]

julia> collect(solve(¬(p ∧ q)))
3-element Vector{Vector{Pair{Variable, Bool}}}:
 [Variable(:p) => 0, Variable(:q) => 1]
 [Variable(:p) => 1, Variable(:q) => 0]
 [Variable(:p) => 0, Variable(:q) => 0]
```

A proposition [`is_satisfiable`](@ref) if there is at least one valid interpretation. A proposition [`is_falsifiable`](@ref) if there is at least one invalid interpretation. A proposition [`is_contingency`](@ref) if it is both satisfiable and falsifiable.

```jldoctest 1
julia> is_satisfiable(p ∧ q) && is_falsifiable(p ∧ q) && is_contingency(p ∧ q)
true
```

A proposition is a tautology if every possible interpretation is true. Likewise, a proposition is a contradiction if every possible interpretation is false. For example, `p ∧ ¬p` is always interpreted as false because either `p` or `¬p` must be false. `p ∨ ¬p` is always interpreted as true because either `p` or `¬p` must be true. Use the functions [`is_tautology`](@ref), [`is_contradiction`](@ref), and [`is_truth`](@ref) to check whether a proposition is logically equivalent to a truth value.

```jldoctest 1
julia> collect(interpretations(p ∧ ¬p))
2-element Vector{Bool}:
 0
 0

julia> collect(interpretations(p ∨ ¬p))
2-element Vector{Bool}:
 1
 1

julia> is_contradiction(p ∧ ¬p) && is_tautology(p ∨ ¬p)
true

julia> is_truth(p ∧ ¬p) && is_truth(p ∨ ¬p)
true
```

Two propositions are logically equivalent if their interpretation is equivalent for every possible valuation. For example, the propositions `¬(¬p ∧ ¬q)` and `p ∨ q` are logically equivalent. In fact, the `or` operator is implemented this way. Use [`==`](@ref) to test that two propositions are logically equivalent.

```jldoctest 1
julia> ¬(¬p ∧ ¬q) == p ∨ q
true

julia> ¬(¬p ∧ ¬q) === p ∨ q
false
```

## Printing

It would also be helpful to enumerate each valuation and interpretation in a visual format. This is accomplished by creating a [`TruthTable`](@ref). A truth table is a table where each column in the header identifies a proposition, and each row contains an interpretation (including the valuation of atomic propositions). To demonstrate these, we will use the [`xor`](@ref) operator, represented by the symbol `⊻`. Try to understand the meaning of this operator as it is interpreted with different valuations.

```jldoctest 1
julia> TruthTable([p ⊻ q])
┌──────────┬──────────┬───────┐
│ p        │ q        │ p ⊻ q │
│ Variable │ Variable │ Tree  │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊤        │ ⊥     │
│ ⊥        │ ⊤        │ ⊤     │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊥        │ ⊤     │
│ ⊥        │ ⊥        │ ⊥     │
└──────────┴──────────┴───────┘
```
