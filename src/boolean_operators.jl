
import Base.⊼, Base.⊽, Base.⊻

"""
    ¬p
    ¬(p)
    not(p)

Logical 'not' operator.

'¬' can be typed by '\\neg<tab>'.

See also [`Not`](@ref).

# Examples
```jldoctest; setup = :(@primitive p)
julia> @truth_table ¬p
┌───────────┬───────────────┐
│         p │            ¬p │
│ Primitive │ Propositional │
├───────────┼───────────────┤
│         ⊤ │             ⊥ │
│         ⊥ │             ⊤ │
└───────────┴───────────────┘
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
```jldoctest; setup = :(@primitive p q)
julia> @truth_table p ∧ q
┌───────────┬───────────┬───────────────┐
│         p │         q │         p ∧ q │
│ Primitive │ Primitive │ Propositional │
├───────────┼───────────┼───────────────┤
│         ⊤ │         ⊤ │             ⊤ │
│         ⊤ │         ⊥ │             ⊥ │
├───────────┼───────────┼───────────────┤
│         ⊥ │         ⊤ │             ⊥ │
│         ⊥ │         ⊥ │             ⊥ │
└───────────┴───────────┴───────────────┘
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
```jldoctest; setup = :(@primitive p q)
julia> @truth_table p ⊼ q
┌───────────┬───────────┬───────────────┐
│         p │         q │         p ⊼ q │
│ Primitive │ Primitive │ Propositional │
├───────────┼───────────┼───────────────┤
│         ⊤ │         ⊤ │             ⊥ │
│         ⊤ │         ⊥ │             ⊤ │
├───────────┼───────────┼───────────────┤
│         ⊥ │         ⊤ │             ⊤ │
│         ⊥ │         ⊥ │             ⊤ │
└───────────┴───────────┴───────────────┘
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
```jldoctest; setup = :(@primitive p q)
julia> @truth_table p ⊽ q
┌───────────┬───────────┬───────────────┐
│         p │         q │         p ⊽ q │
│ Primitive │ Primitive │ Propositional │
├───────────┼───────────┼───────────────┤
│         ⊤ │         ⊤ │             ⊥ │
│         ⊤ │         ⊥ │             ⊥ │
├───────────┼───────────┼───────────────┤
│         ⊥ │         ⊤ │             ⊥ │
│         ⊥ │         ⊥ │             ⊤ │
└───────────┴───────────┴───────────────┘
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
```jldoctest; setup = :(@primitive p q)
julia> @truth_table p ∨ q
┌───────────┬───────────┬───────────────┐
│         p │         q │         p ∨ q │
│ Primitive │ Primitive │ Propositional │
├───────────┼───────────┼───────────────┤
│         ⊤ │         ⊤ │             ⊤ │
│         ⊤ │         ⊥ │             ⊤ │
├───────────┼───────────┼───────────────┤
│         ⊥ │         ⊤ │             ⊤ │
│         ⊥ │         ⊥ │             ⊥ │
└───────────┴───────────┴───────────────┘
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
```jldoctest; setup = :(@primitive p q)
julia> @truth_table p ⊻ q
┌───────────┬───────────┬───────────────┐
│         p │         q │         p ⊻ q │
│ Primitive │ Primitive │ Propositional │
├───────────┼───────────┼───────────────┤
│         ⊤ │         ⊤ │             ⊥ │
│         ⊤ │         ⊥ │             ⊤ │
├───────────┼───────────┼───────────────┤
│         ⊥ │         ⊤ │             ⊤ │
│         ⊥ │         ⊥ │             ⊥ │
└───────────┴───────────┴───────────────┘
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
```jldoctest; setup = :(@primitive p q)
julia> @truth_table p → q
┌───────────┬───────────┬───────────────┐
│         q │         p │         p → q │
│ Primitive │ Primitive │ Propositional │
├───────────┼───────────┼───────────────┤
│         ⊤ │         ⊤ │             ⊤ │
│         ⊤ │         ⊥ │             ⊤ │
├───────────┼───────────┼───────────────┤
│         ⊥ │         ⊤ │             ⊥ │
│         ⊥ │         ⊥ │             ⊤ │
└───────────┴───────────┴───────────────┘
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
```jldoctest; setup = :(@primitive p q)
julia> @truth_table p ← q
┌───────────┬───────────┬───────────────┐
│         p │         q │         p ← q │
│ Primitive │ Primitive │ Propositional │
├───────────┼───────────┼───────────────┤
│         ⊤ │         ⊤ │             ⊤ │
│         ⊤ │         ⊥ │             ⊤ │
├───────────┼───────────┼───────────────┤
│         ⊥ │         ⊤ │             ⊥ │
│         ⊥ │         ⊥ │             ⊤ │
└───────────┴───────────┴───────────────┘
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
```jldoctest; setup = :(@primitive p q)
julia> @truth_table p ↔ q
┌───────────┬───────────┬───────────────┐
│         q │         p │         p ↔ q │
│ Primitive │ Primitive │ Propositional │
├───────────┼───────────┼───────────────┤
│         ⊤ │         ⊤ │             ⊤ │
│         ⊤ │         ⊥ │             ⊥ │
├───────────┼───────────┼───────────────┤
│         ⊥ │         ⊤ │             ⊥ │
│         ⊥ │         ⊥ │             ⊤ │
└───────────┴───────────┴───────────────┘
```
"""
function only_if end
const ↔ = only_if
p ↔ q = (p → q) ∧ (p ← q)
