
import Base.⊼, Base.⊽, Base.⊻

# data types

"""
    Language

Supertype of [`Primitive`](@ref) and [`Compound`](@ref).
"""
abstract type Language end

"""
    Compound <: Language

Supertype of [`Propositional`](@ref) and [`Modal`](@ref).
Subtype of [`Language`](@ref).
"""
abstract type Compound <: Language end

"""
    Primitive{S <: Union{String, Nothing}} <: Language
    Primitive{S}([s = nothing])

Primitive proposition.

See also [`Language`](@ref) and [`Compound`](@ref).

# Usage
```jldoctest
julia> # p = Primitive("Logic is fun")

julia> # (¬p)()

```
"""
struct Primitive{S <: Union{String, Nothing}} <: Language
    statement::S

    Primitive(s::S = nothing) where S = new{S}(s)
end

"""
    Valuation{V <: Union{Val{:⊥}, Val{:⊤}}, P <: Union{Primitive, Vector{<:Primitive}}}
    Valuation(::V, [p::P = Primitive()])
    Valuation(v, ps...)

Container for [`Tautology`](@ref) and [`Contradiction`](@ref) of [`Primitive`](@ref).
"""
struct Valuation{V <: Union{Val{:⊥}, Val{:⊤}}, P <: Union{Primitive, Vector{<:Primitive}}}
    p::P

    Valuation(::V, p::P = Primitive()) where {V, P} = new{V, P}(p)
end

Valuation(::V, p) where V <: Valuation{<:Val} = Valuation(first(V.parameters)(), p)
Valuation(v, ps...) = Valuation(v, collect(ps))

"""
    ⊥
    Contradiction

The constant 'Contradiction'.

'⊥' can be typed by '\\bot<tab>'.

See also [`Valuation`](@ref) and [`Tautology`](@ref).

# Usage
```jldoctest
julia> # ¬⊥

julia> # Propositional(Contradiction)()
```
"""
const Contradiction = Valuation(Val(:⊥))
const ⊥ = Contradiction

"""
    ⊤
    Tautology

The constant 'Tautology'.

'⊤' can be typed by '\\top<tab>'.

See also [`Valuation`](@ref) and [`Contradiction`](@ref).

# Usage
```jldoctest
julia> # ¬⊤

julia> # Propositional(Tautology)()
```
"""
const Tautology = Valuation(Val(:⊤))
const ⊤ = Tautology

"""
Operator

Supertype of [`Boolean`](@ref) and [`Modal`](@ref).
"""
abstract type Operator end

"""
    Boolean <: Operator

Supertype of [`Not`](@ref) and [`And`](@ref). Together, these two connectives are functionally complete.

See also [`Operator`](@ref) and [Boolean Operators](@ref).
"""
abstract type Boolean <: Operator end

"""
    Not <: Boolean

Singleton type representing logical negation.

See also [`Boolean`](@ref), [`Propositional`](@ref), and [`And`](@ref).

# Usage
```jldoctest
julia> # Not()(⊤)

julia> # Not()(⊥)
```
"""
struct Not <: Boolean end
const _not = Not()
(::Not)(p::Valuation{Val{:⊤}}) = Valuation(⊥, p.p)
(::Not)(p::Valuation{Val{:⊥}}) = Valuation(⊤, p.p)
(::Not)(p::Primitive) = ¬Propositional(p)
(::Not)(p::Compound) = Propositional((_not, p))

"""
    And <: Boolean

Singleton type representing logical conjunction.

See also [`Boolean`](@ref), [`Propositional`](@ref), and [`Not`](@ref).

# Usage
```jldoctest
julia> # And()(⊤, ⊤)

julia> # And()(⊤, ⊥)

julia> # And()(⊥, ⊤)

julia> # And()(⊥, ⊥)
```
"""
struct And <: Boolean end
const _and = And()
(::And)(p::Valuation{Val{:⊤}}, q::Valuation{Val{:⊤}}) = Valuation(⊤, (collect(Set([p.p; q.p]))))
(::And)(p::Valuation, q::Valuation) = Valuation(⊥, (collect(Set([p.p; q.p]))))

"""
    Propositional{
        L <: Union{
            Primitive,
            Tuple{Not, Language},
            Tuple{And, Language, Language}
        }
    } <: Compound
    Propositional{L}(ϕ)

Abstract syntax tree representing a compound proposition.

See also [`Language`](@ref), [`Compound`](@ref), [`Primitive`](@ref), [`Not`](@ref), and [`And`](@ref).

# Examples
```jldoctest
julia> # p = Propositional(Primitive())

julia> # (¬p)()

```
"""
struct Propositional{
    L <: Union{
        Primitive,
        Tuple{Not, Compound},
        Tuple{And, Compound, Compound}
    }
} <: Compound
    ϕ::L
end

Propositional(::Valuation{Val{:⊥}}, p = Primitive()) = p ∧ ¬p
Propositional(::Valuation{Val{:⊤}}, p = Primitive()) = ¬Propositional(⊥, p)
(v::Valuation)() = Propositional(v)

# boolean operators

"""
    ¬p
    ¬(p)
    not(p)

Logical 'not' operator.

'¬' can be typed by '\\neg<tab>'.

See also [`Not`](@ref).

# Examples
```jldoctest
julia> # ¬⊤

julia> # not(⊥)
```
"""
function not end
const ¬ = not
¬p = _not(p)

"""
    p ∧ q
    ∧(p, q)
    and(p, q)

Logical 'and' operator.

'∧' can be typed by '\\wedge<tab>'.

See also [`And`](@ref).

# Examples
```jldoctest
julia> # ⊤ ∧ ⊤

julia> # ⊤ ∧ ⊥

julia> # ∧(⊥, ⊤)

julia> # and(⊥, ⊥)
```
"""
function and end
const ∧ = and
p ∧ q = _and(p, q)

"""
    p ⊼ q
    ⊼(p, q)
    nand(p, q)

Logical 'nand' operator.

'⊼' can be typed by '\\nand<tab>'.

# Examples
```jldoctest
julia> # ⊤ ⊼ ⊤

julia> # ⊤ ⊼ ⊥

julia> # ⊼(⊥, ⊤)

julia> # nand(⊥, ⊥)
```
"""
nand
p ⊼ q = ¬(p ∧ q)

"""
    p ⊽ q
    ⊽(p, q)
    nor(p, q)

Logical 'nor' operator.

'⊽' can be typed by '\\nor<tab>'.

# Examples
```jldoctest
julia> # ⊤ ⊽ ⊤

julia> # ⊤ ⊽ ⊥

julia> # ⊽(⊥, ⊤)

julia> # nor(⊥, ⊥)
```
"""
nor
p ⊽ q = ¬p ∧ ¬q

"""
    p ∨ q
    ∨(p, q)
    or(p, q)

Logical 'or' operator.

'∨' can be typed by '\\vee<tab>'.

# Examples
```jldoctest
julia> # ⊤ ∨ ⊤

julia> # ⊤ ∨ ⊥

julia> # ∨(⊥, ⊤)

julia> # or(⊥, ⊥)
```
"""
function or end
const ∨ = or
p ∨ q = ¬(p ⊽ q)

"""
    p ⊻ q
    ⊻(p, q)
    xor(p, q)

Logical 'xor' operator.

'⊻' can be typed by '\\xor<tab>'.

# Examples
```jldoctest
julia> # ⊤ ⊻ ⊤

julia> # ⊤ ⊻ ⊥

julia> # ⊻(⊥, ⊤)

julia> # xor(⊥, ⊥)
```
"""
xor
p ⊻ q = (p ∨ q) ∧ (p ⊼ q)

"""
    p → q
    →(p, q)
    if_then(p, q)

Logical 'if_then' operator.

'→' can be typed by '\\rightarrow<tab>'.

# Examples
```jldoctest
julia> # ⊤ → ⊤

julia> # ⊤ → ⊥

julia> # →(⊥, ⊤)

julia> # if_then(⊥, ⊥)
```
"""
function if_then end
const → = if_then
p → q = ¬(p ∧ ¬q)

"""
    p ← q
    ←(p, q)
    then_if(p, q)

Logical 'then_if' operator.

'←' can be typed by '\\leftarrow<tab>'.

# Examples
```jldoctest
julia> # ⊤ ← ⊤

julia> # ⊤ ← ⊥

julia> # ←(⊥, ⊤)

julia> # then_if(⊥, ⊥)
```
"""
function then_if end
const ← = then_if
p ← q = q → p

"""
    p ↔ q
    ↔(p, q)
    only_if(p, q)

Logical 'only_if' operator.

'↔' can be typed by '\\leftrightarrow<tab>'.

# Examples
```jldoctest
julia> # ⊤ ↔ ⊤

julia> # ⊤ ↔ ⊥

julia> # ↔(⊥, ⊤)

julia> # only_if(⊥, ⊥)
```
"""
function only_if end
const ↔ = only_if
p ↔ q = (p → q) ∧ (p ← q)
