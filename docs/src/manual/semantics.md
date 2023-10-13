
# Semantics

!!! warning
    The algorithm to solve propositions currently has a naive implementation with a runtime of [`O(2 ^ n)`](https://en.wikipedia.org/wiki/Big_O_notation) where [`n = length(unique(atoms(p)))`](@ref atoms).

## Properties

```@docs
dual
converse
left_neutrals
right_neutrals
```

## Truths

```@docs
valuations
interpret
interpretations
solve
```

## Predicates

```@docs
==
is_tautology
is_contradiction
is_truth
is_contingency
is_satisfiable
is_falsifiable
```

## Utilities

```@docs
convert
```
