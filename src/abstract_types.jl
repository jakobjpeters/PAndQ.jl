
"""
Language

Set of well-formed logical formulae.

Calling an instance of ```Language``` will return a vector of valid interpretations.

Supertype of [`Primitive`](@ref), [`Compound`](@ref), and [`Truth`](@ref).
```
"""
abstract type Language end

"""
Compound <: Language

Compound proposition.

Subtype of [`Language`](@ref).
Supertype of [`Propositional`](@ref).
"""
abstract type Compound <: Language end

"""
    Operator

Set of functions that operate on a logical [`Language`](@ref).

Supertype of [`Boolean`](@ref).
"""
abstract type Operator end
