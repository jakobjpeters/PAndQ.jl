
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
