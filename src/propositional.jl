
import Base.⊼, Base.⊽, Base.⊻

"""
    Primitive{S <: Union{String, Nothing}}
    Primitive(s = nothing)

Primitive proposition.

See also [`Proposition`](@ref).

# Usage
```jldoctest
julia> p = Primitive("Logic is fun")
Primitive{String}("Logic is fun")

julia> ¬p
Language(
  Not(), Language(
    Primitive{String}("Logic is fun")
  )
)
```
"""
struct Primitive{S <: Union{String, Nothing}}
    statement::S

    Primitive(s::S = nothing) where S = new{S}(s)
end

(p::Primitive)(states) = get(states, p, Set([p]))

"""
    @primitive

Instantiate 'Primitive' propositions.

# Examples
```
julia> @primitive p q

julia> p
Primitive{String}("p")

julia> q
Primitive{String}("q")
```
"""
macro primitive(expressions...)
    primitives = map(expression -> :($(esc(expression)) = Primitive($(string(expression)))), expressions)
    return quote
        $(primitives...)
        nothing
    end
end
#=
Source:
https://github.com/ctrekker/Deductive.jl
=#

"""
    Valuation

Supertype of [`⊤`](@ref) and [`⊥`](@ref).
"""
abstract type Valuation end

"""
    ⊥ <: Valuation

Contradiction/false, depending on context.

See also [`Contradiction`](@ref).

# Usage
```jldoctest
julia> ¬⊥
⊤

julia> Proposition(⊥)
Language(
  And(), Language(
    Primitive{Nothing}(nothing)
  ) Language(
    Not(), Language(
      Primitive{Nothing}(nothing)
    )
  )
)
```
"""
struct ⊥ <: Valuation end

"""
    Contradiction <: Valuation

Alias for [`⊥`](@ref).
"""
Contradiction = ⊥

"""
    ⊤ <: Valuation

Tautology/true, depending on context.

See also [`Tautology`](@ref).

# Usage
```jldoctest
julia> ¬⊤
⊥

julia> Proposition(⊤)
Language(
  Not(), Language(
    And(), Language(
      Primitive{Nothing}(nothing)
    ) Language(
      Not(), Language(
        Primitive{Nothing}(nothing)
      ) 
    ) 
  ) 
)
```
"""
struct ⊤ <: Valuation end

"""
    Tautology <: Valuation

Alias for [`⊤`](@ref).
"""
Tautology = ⊤

"""
    Boolean <: Operator

Supertype of [`Not`](@ref) and [`And`](@ref).
"""
abstract type Boolean <: Operator end

"""
    Not <: Boolean

Singleton type representing logical negation.

See also [`Proposition`](@ref), [`¬`](@ref), and [`not`](@ref).

# Usage
```jldoctest
julia> Not()(⊤)
⊥

julia> Not()(⊥)
⊤
```
"""
struct Not <: Boolean end
(::Not)(::Type{⊥}) = ⊤
(::Not)(::Type{⊤}) = ⊥
(::Not)(p) = p

"""
    And <: Boolean

Singleton type representing logical conjunction.

See also [`Proposition`](@ref), [`∧`](@ref), and [`And`](@ref).

# Usage
```jldoctest
julia> And()(⊤, ⊤)
⊤

julia> And()(⊤, ⊥)
⊥

julia> And()(⊥, ⊤)
⊥

julia> And()(⊥, ⊥)
⊥
```
"""
struct And <: Boolean end
(::And)(::Type{⊤}, ::Type{⊤}) = ⊤
(::And)(p::Type, q::Type) = ⊥
(::And)(p, q) = union(p, q)

"""
    Proposition{T <: Union{
        Primitive,
        Tuple{Not, Language},
        Tuple{And, Language, Language}
    }} <: Language

Abstract syntax tree for propositional logic.

# Examples
```
julia> p = Proposition(Primitive())
Language(
    Primitive{Nothing}(nothing)
)

julia> # (¬p)()

```
"""
struct Proposition{T <: Union{
    Primitive,
    Tuple{Not, Language},
    Tuple{And, Language, Language}
}} <: Language
    ϕ::T
end

Proposition(::Type{⊥}, p = Primitive()) = p ∧ ¬p
Proposition(::Type{⊤}, p = Primitive()) = ¬Proposition(⊥, p)

# logical operators

const _not = Not()
const _and = And()

"""
    ¬p

Logical 'not' operator.

'¬' can be typed by '\\neg<tab>'.

See also [`not`](@ref).

# Examples
```jldoctest
julia> ¬⊤
⊥

julia> ¬⊥
⊤
```
"""
¬(p::Type{<:Valuation}) = _not(p)
¬(p::Primitive) = ¬Proposition(p)
¬(p::Proposition) = Proposition((_not, p))

"""
    not(p)

Alias for [`¬`](@ref).
"""
not = ¬

"""
    p ∧ q

Logical 'and' operator.

'∧' can be typed by '\\wedge<tab>'.

See also [`and`](@ref).

# Examples
```jldoctest
julia> ⊤ ∧ ⊤
⊤

julia> ⊤ ∧ ⊥
⊥

julia> ⊥ ∧ ⊤
⊥

julia> ⊥ ∧ ⊥
⊥
```
"""
∧(p::Type{<:Valuation}, q::Type{<:Valuation}) = _and(p, q)
∧(p, q) = Proposition(p) ∧ q
∧(p::Language, q) = q ∧ p
∧(p::Proposition, q::Proposition) = Proposition(((_and, p, q)))

"""
    and(p, q)

Alias for [`∧`](@ref).
"""
and = ∧

"""
    p ⊼ q

Logical 'nand' operator.

'⊼' can be typed by '\\nand<tab>'.

See also [`nand`](@ref).

# Examples
```jldoctest
julia> ⊤ ⊼ ⊤
⊥

julia> ⊤ ⊼ ⊥
⊤

julia> ⊥ ⊼ ⊤
⊤

julia> ⊥ ⊼ ⊥
⊤
```
"""
⊼(p, q) = ¬(p ∧ q)

"""
    nand(p, q)

Alias for [`⊼`](@ref).
"""
nand = ⊼

"""
    p ⊽ q

Logical 'nor' operator.

'⊽' can be typed by '\\nor<tab>'.

See also [`nor`](@ref).

# Examples
```jldoctest
julia> ⊤ ⊽ ⊤
⊥

julia> ⊤ ⊽ ⊥
⊥

julia> ⊥ ⊽ ⊤
⊥

julia> ⊥ ⊽ ⊥
⊤
```
"""
⊽(p, q) = ¬p ∧ ¬q

"""
    nor(p, q)

Alias for [`⊽`](@ref).
"""
nor = ⊽

"""
    p ∨ q

Logical 'or' operator.

'∨' can be typed by '\\vee<tab>'.

See also [`∨`](@ref).

# Examples
```jldoctest
julia> ⊤ ∨ ⊤
⊤

julia> ⊤ ∨ ⊥
⊤

julia> ⊥ ∨ ⊤
⊤

julia> ⊥ ∨ ⊥
⊥
```
"""
∨(p, q) = ¬(p ⊽ q)

"""
    or(p, q)

Alias for [`∨`](@ref).
"""
or = ∨

"""
    p ⊻ q

Logical 'xor' operator.

'⊻' can be typed by '\\xor<tab>'.

See also [`xor`](@ref).

# Examples
```jldoctest
julia> ⊤ ⊻ ⊤
⊥

julia> ⊤ ⊻ ⊥
⊤

julia> ⊥ ⊻ ⊤
⊤

julia> ⊥ ⊻ ⊥
⊥
```
"""
⊻(p, q) = (p ∨ q) ∧ (p ⊼ q)

"""
    xor(p, q)

Alias for [`⊻`](@ref).
"""
xor = ⊻

"""
    p → q

Logical 'if_then' operator.

'→' can be typed by '\\rightarrow<tab>'.

See also [`if_then`](@ref).

# Examples
```jldoctest
julia> ⊤ → ⊤
⊤

julia> ⊤ → ⊥
⊥

julia> ⊥ → ⊤
⊤

julia> ⊥ → ⊥
⊤
```
"""
→(p, q) = ¬(p ∧ ¬q)

"""
    if_then(p, q)

Alias for [`→`](@ref).
"""
if_then = →

"""
    p ← q

Logical 'then_if' operator.

'←' can be typed by '\\leftarrow<tab>'.

See also [`then_if`](@ref).

# Examples
```jldoctest
julia> ⊤ ← ⊤
⊤

julia> ⊤ ← ⊥
⊤

julia> ⊥ ← ⊤
⊥

julia> ⊥ ← ⊥
⊤
```
"""
←(p, q) = q → p

"""
    then_if(p, q)

Alias for [`←`](@ref).
"""
then_if = ←

"""
    p ↔ q

Logical 'only_if' operator.

'↔' can be typed by '\\leftrightarrow<tab>'.

See also [`only_if`](@ref).

# Examples
```jldoctest
julia> ⊤ ↔ ⊤
⊤

julia> ⊤ ↔ ⊥
⊥

julia> ⊥ ↔ ⊤
⊥

julia> ⊥ ↔ ⊥
⊤
```
"""
↔(p, q) = (p → q) ∧ (p ← q)

"""
    only_if(p, q)

Alias for [`↔`](@ref).
"""
only_if = ↔

length(p::Primitive) = 1
length(ϕ::Tuple{Boolean, Vararg}) = 1 + mapreduce(length, +, Base.tail(ϕ))

print(p::Primitive, indent = 0) = print(repeat("  ", indent), p)
