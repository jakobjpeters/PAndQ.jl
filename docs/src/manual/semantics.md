
# Semantics

!!! warning
    The model checking algorithm currently has a naive implementation with a [time complexity](https://en.wikipedia.org/wiki/Big_O_notation) of [`O(2 ^ length(unique(atoms(p))))`](@ref atoms).

## Truths

```@docs
valuations
map
interpret
interpretations
solve
```

## Predicates

```@docs
is_commutative
is_associative
is_tautology
is_contradiction
is_truth
is_contingency
is_satisfiable
is_falsifiable
```

### Ordering

Propositions and their truth values have a [strict partial order](https://en.wikipedia.org/wiki/Partially_ordered_set#strict_partial_order).
The truth values [`tautology`](@ref) and [`contradiction`](@ref) are the top and bottom of this order, respectively.
A proposition that satisfies the predicate [`is_tautology`](@ref) or [`is_contradiction`](@ref) is also at the top or bottom of the order, respectively.
Propositions that satisfy the predicate `is_contingency` occupy the middle of this order.
More specifically, `⊥ <= p <= ⊤` for some proposition `p`.
The ordering is partial because the predicates [`==`](@ref) and [`is_truth`](@ref) may both be false for two given propositions.

!!! note
    The implementations for `==` and `<` also define the semantics of `isequal`, `>`, `<=`, and `>=`.
    This does not define the semantics of `isless`, which is used for [total orders](https://en.wikipedia.org/wiki/Total_order).

!!! warning
    The assumption that `isequal(p, q)` implies `hash(p) == hash(q)` is currently being violated.
    The implementation of `hash` for propositions is in-progress.

```@docs
==
<
```

## Properties

```@docs
dual
converse
left_neutrals
right_neutrals
```

## Utilities

```@docs
Bool(::PAndQ.NullaryOperator)
convert(::Type{Bool}, ::typeof(⊤))
```
