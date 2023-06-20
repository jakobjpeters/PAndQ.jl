
```@meta
DocTestSetup = :(using PAQ)
```

# Tutorial

## Truth Values

A truth value is logic's version of a boolean `true` or `false`. These concepts can also be represented as a `1` or a `0`. Similarly, we use [`tautology`](@ref) and [`contradiction`](@ref). These are commonly represented with the symbols `⊤` and `⊥`.

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

There are several more operators, which will be discussed later.

## Propositions

A proposition is a statement that can be either true or false. For example, "Logic is fun" is a proposition because
it may be true for you but false for someone else. Note that the proposition exists on its own, regardless of whether or not it is known to be true or false. We can also perform operations on propositions. In a written form, we can negate the above proposition by saying "Logic is not fun". We could combine two propositions using another operator, such as "Logic is fun and Julia is awesome". An [`Atom`](@ref)ic proposition is such that it has not been
operated on and is not composed of any other propositions. Thus the first proposition, "Logic is fun", is atomic.
Notice that the other two propositions, "Logic is not fun" and "Logic is fun and Julia is awesome", are [`Compound`](@ref) propositions.

```jldoctest 1
julia> p = Atom("Logic is fun")
Atom:
 "Logic is fun"

julia> q = Atom("Julia is awesome")
Atom:
 "Julia is awesome"

julia> r = ¬p
Literal:
 ¬"Logic is fun"

julia> s = p ∧ q
Tree:
 "Logic is fun" ∧ "Julia is awesome"

julia> p isa Atom && q isa Atom
true

julia> r isa Compound && s isa Compound
true
```

We know that since these are propositions, they can be true or false. It is simple to evaluate atomic propositions:
`p` is true if you think that "Logic is fun" and is false otherwise. Assigning meaning to any number of atomic propositions is called a [`Valuation`](@ref get_valuations). Since `p` is can only be true or false, it has two possible valuations. If you think that "Logic is fun", it would be invalid to assign it the valuation false. `r` doesn't depend on any other propositions, so it also has two possible valuations. However, the valuation and result of evaluating it are not longer the same. Since `r == ¬p`, if `p` is assigned true, then `r` is determined to be false. An [`interpretation`](@ref get_interpretations) is the truth values of any number of propositions determined by a given valuation.

```jldoctest 1
julia> get_valuations(r)
2-element Vector{Vector}:
 Pair{Atom{String}, typeof(tautology)}["Logic is fun" => ⊤]
 Pair{Atom{String}, typeof(contradiction)}["Logic is fun" => ⊥]

julia> get_interpretations(r)
2-element Vector{Function}:
 contradiction (generic function with 1 method)
 tautology (generic function with 1 method)
```

## Symbolic Logic

In mathematics, it's useful to replace individual numbers with a symbolic variable that can represent any number. Since these propositions are for demonstration and could really be any proposition, we will do the same with our propositions `p` and `q`. To do so, we will use the [`@atoms`](@ref) macro to define each atomic proposition as a `const`ant.

```jldoctest 2
julia> @atoms p q
2-element Vector{Atom{Symbol}}:
 p
 q

julia> p
Atom:
 p

julia> q
Atom:
 q

julia> s = p ∧ q
Tree:
 p ∧ q
```

Since `s` contains two atomic propositions, there are four valuations: `p` is true and `q` is true, `p` is false and `q` is true, `p` is true and `q` is false, and `p` is false and `q` is false. Each additional atomic proposition in a proposition doubles the number of possible valuations. Mathematically, if [`n = length(get_atoms(p))`](@ref get_atoms), then there are ``\(2 ^ n\)`` valuations. Since each interpretation depends on a valuation, the number of valuations and interpretations are equal.

```jldoctest 2
julia> atoms = get_atoms(s)
2-element Vector{Atom{Symbol}}:
 p
 q

julia> n = length(atoms)
2

julia> length(get_valuations(s)) == length(get_interpretations(s)) == 2 ^ n == 4
true
```

TODO: write paragraph

```jldoctest 2
julia> interpret(s, p => ⊤)
Normal:
 (q)

julia> interpret(s, p => ⊤, q => ⊤)
tautology (generic function with 1 method)
```

We are often interested in valuations that result in a valid interpretation. This is accomplished with the [`solve`](@ref) function. It would also be helpful to enumerate each valuation and interpretation in a visual format. This is accomplished by creating a [`TruthTable`](@ref). A truth table is a table where each column in the header identifies a proposition, and each row contains an interpretation (including the valuation of atomic propositions). To demonstrate these, we will use the [`xor`](@ref) operator, represented by the symbol `⊻`. Try to understand the meaning of this operator as it is interpreted with different valuations.

```jldoctest 2
julia> solve(p ⊻ q)
2-element Vector{Vector{Pair{Atom{Symbol}}}}:
 [p => ⊥, q => ⊤]
 [p => ⊤, q => ⊥]

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

TODO: write paragraph

```jldoctest 2
julia> is_contradiction(p ∧ ¬p)
true

julia> is_tautology(p ∨ ¬p)
true

julia> TruthTable(p ∧ ¬p, p ∨ ¬p)
┌────────┬────────┬──────┐
│ p ∧ ¬p │ p ∨ ¬p │ p    │
│ Tree   │ Tree   │ Atom │
├────────┼────────┼──────┤
│ ⊥      │ ⊤      │ ⊤    │
│ ⊥      │ ⊤      │ ⊥    │
└────────┴────────┴──────┘
```
