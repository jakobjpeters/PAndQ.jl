
# Semantics

## Truths

```@docs
valuations
interpret
interpretations
solutions
```

## Predicates

```@docs
is_tautology
is_contradiction
is_truth
is_contingency
is_satisfiable
is_falsifiable
is_equisatisfiable
```

### Ordering

Propositions and their truth values have a [strict partial order](https://en.wikipedia.org/wiki/Partially_ordered_set#strict_partial_order).
The truth values [`tautology`](@ref) and [`contradiction`](@ref) are the top and bottom of this order, respectively.
A proposition that satisfies the predicate [`is_tautology`](@ref) or [`is_contradiction`](@ref) is also at the top or bottom of the order, respectively.
Propositions that satisfy the predicate [`is_contingency`](@ref) occupy the middle of this order.
In other words, `⊥ < ⊤`, `⊥ <= p`, and `p <= ⊤` for some proposition `p`.
The ordering is partial because the predicates [`==`](@ref) and [`is_truth`](@ref) may both be false for two given propositions.

!!! note
    The implementations for `==` and `<` also define the semantics of `isequal`, `>`, `<=`, and `>=`.
    This does not define the semantics of `isless`, which is used for [total orders](https://en.wikipedia.org/wiki/Total_order).

!!! warning
    The assumption that `isequal(p, q)` implies `hash(p) == hash(q)` is currently being violated.
    A future version will implement the Quine–McCluskey algorithm to minimize propositions,
    which will enable `hash` to be defined too.

```@docs
==
<
```

## Utilities

```@docs
Bool(::PAndQ.NullaryOperator)
```
