
# Semantics

!!! warning
    The algorithm to find the interpretations of a [`Proposition`](@ref PAndQ.Proposition)
    currently has a naive implementation with a runtime of
    [`O(2 ^ n)`](https://en.wikipedia.org/wiki/Big_O_notation)
    where [`n = length(unique(atoms(p)))`](@ref atoms).

## Truths

```@docs
valuations
interpret
interpretations
solve
```

## Predicates

```@docs
is_commutative
is_associative
==
is_tautology
is_contradiction
is_truth
is_contingency
is_satisfiable
is_falsifiable
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
