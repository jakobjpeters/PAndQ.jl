
```@meta
DocTestSetup = :(using PAndQ)
```

# Getting Started

This page demonstrates the basic functionality of this package. For additional features and documentation, see the Tutorials and Manual.

## Operators

[Operators](@ref operators_operators) are functions represented by a symbol that return a logical value.

The [`tautology`](@ref) and [`contradiction`](@ref) operators are truth values similar to the `Bool`ean values `true` and `false`, respectively. These operators are represented with the symbols `⊤` and `⊥` and return themselves when called.

```jldoctest
julia> ⊤()
⊤

julia> ⊥()
⊥
```

The symbols `!` and `¬` both represent the unary operator [`not`](@ref). The symbols `&` and `∧` represent the binary operator [`and`](@ref).

```jldoctest
julia> !true
false

julia> true & false
false

julia> ¬⊤
¬⊤

julia> ⊤ ∧ ⊥
⊤ ∧ ⊥
```

## Propositions

[Propositions]() are statements that can be either `true` or `false`. For example, "Logic is fun". This proposition has a known value, so it is a constant. Note that the proposition exists independently of whether it is known to be `true` or `false`. Constants can be instantiated inline with the [`@atomize`](@ref) macro and unwrapped with the [`value`](@ref) function.

```jldoctest 1
julia> p = @atomize $"Logic is fun"
$("Logic is fun")

julia> q = @atomize $"Julia is awesome"
$("Julia is awesome")

julia> map(something ∘ value, [p, q])
2-element Vector{String}:
 "Logic is fun"
 "Julia is awesome"
```

A proposition can be negated, such as "Logic is not fun". Multiple propositions can be connected, such as "Logic is fun and Julia is awesome".

```jldoctest 1
julia> ¬p
¬$("Logic is fun")

julia> p ∧ q
$("Logic is fun") ∧ $("Julia is awesome")
```

Variables represent a proposition with an unknown value. Use the `@atomize` macro to instantiate them inline or the [`@variables`](@ref) macro to define multiple variables at once.

```jldoctest 1
julia> @variables p q
2-element Vector{PAndQ.AbstractSyntaxTree}:
 p
 q

julia> ¬p
¬p

julia> p ∧ q
p ∧ q
```

Operators always eagerly evaluate boolean values. Boolean values cannot interoperate with truth values and propositions.

```jldoctest 1
julia> true ∧ true
true

julia> ⊤ ∧ p
⊤ ∧ p
```

## Semantics

Constants and variables are atomic propositions. Operators construct compound propositions from one or more atomic propositions. Each atom in a proposition can be assigned the valuation `true` or `false`. This results in an interpretation, which determines the truth value of the overall proposition. For example, assigning the valuation `true` to the atomic proposition "Logic is fun" determines that the compound proposition "Logic is not fun" is interpreted as `false`. Use the [`interpret`](@ref) function to assign truth values to atomic propositions.

```jldoctest 1
julia> interpret([p => ⊤], p ∧ q)
⊤ ∧ q

julia> interpret([p => ⊤, q => ⊥], p ∧ q)
⊤ ∧ ⊥
```

The [`solutions`](@ref) of a proposition are the [`valuations`](@ref) that result in a `true` interpretation.

```jldoctest 1
julia> collect(valuations(p ∧ q))
2×2 Matrix{Vector{Pair{PAndQ.AbstractSyntaxTree, Bool}}}:
 [AbstractSyntaxTree(:p)=>1, AbstractSyntaxTree(:q)=>1]  …  [AbstractSyntaxTree(:p)=>1, AbstractSyntaxTree(:q)=>0]
 [AbstractSyntaxTree(:p)=>0, AbstractSyntaxTree(:q)=>1]     [AbstractSyntaxTree(:p)=>0, AbstractSyntaxTree(:q)=>0]

julia> solutions(p ∧ q)[1]
2-element Vector{PAndQ.AbstractSyntaxTree}:
 q
 p

julia> collect(only(solutions(p ∧ q)[2]))
2-element Vector{Bool}:
 1
 1
```

Two propositions are logically equivalent if their interpretation is equivalent for every possible valuation. Use [`is_equivalent`](@ref) to check if two propositions are logically equivalent.

```jldoctest 1
julia> is_equivalent(p ∧ ¬p, ⊥)
true

julia> p ∧ ¬p == ⊥
false
```

## Printing

[`TruthTable`](@ref)s are used to enumerate the interpretations of propositions. The header contains propositions and the atoms composing them. Each column corresponds to the truth values of the proposition in the header. Each row represents an interpretation. Use the [`print_table`](@ref) function to print a truth table.

```jldoctest 1
julia> print_table(⊤, ¬p, p ∧ q)
┌───┬───┬───┬────┬───────┐
│ ⊤ │ p │ q │ ¬p │ p ∧ q │
├───┼───┼───┼────┼───────┤
│ ⊤ │ ⊤ │ ⊤ │ ⊥  │ ⊤     │
│ ⊤ │ ⊥ │ ⊤ │ ⊤  │ ⊥     │
├───┼───┼───┼────┼───────┤
│ ⊤ │ ⊤ │ ⊥ │ ⊥  │ ⊥     │
│ ⊤ │ ⊥ │ ⊥ │ ⊤  │ ⊥     │
└───┴───┴───┴────┴───────┘
```
