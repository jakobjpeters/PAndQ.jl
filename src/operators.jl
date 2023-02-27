
import Base: identity, nand, ⊼, nor, ⊽, xor, ⊻

"""
    ⊤()
    tautology()

A constant which is [true in every possible interpretation](https://en.wikipedia.org/wiki/Tautology_(logic)).

```⊤``` can be typed by ```\\top<tab>```.

# Examples
```jldoctest
julia> ⊤()
tautology (generic function with 1 method)

julia> @truth_table ⊤
┌────────┐
│ ⊤      │
│ Clause │
├────────┤
│ ⊤      │
└────────┘
```
"""
function tautology end
const ⊤ = tautology

"""
    ⊥()
    contradiction()

A constant which is [false in every possible interpretation](https://en.wikipedia.org/wiki/Contradiction).

```⊥``` can be typed by ```\\bot<tab>```.

# Examples
```jldoctest
julia> ⊥()
contradiction (generic function with 1 method)

julia> @truth_table ⊥
┌────────┐
│ ⊥      │
│ Clause │
├────────┤
│ ⊥      │
└────────┘
```
"""
function contradiction end
const ⊥ = contradiction

"""
    identity(::Proposition)

# Examples
```jldoctest
julia> @truth_table p
┌──────┐
│ p    │
│ Atom │
├──────┤
│ ⊤    │
│ ⊥    │
└──────┘
```
"""
function identity end

"""
    ¬p
    ¬(p)
    not(p)

Logical [negation](https://en.wikipedia.org/wiki/Negation) operator.

```¬``` can be typed by ```\\neg<tab>```.

# Examples
```jldoctest
julia> @truth_table ¬p
┌──────┬─────────┐
│ p    │ ¬p      │
│ Atom │ Literal │
├──────┼─────────┤
│ ⊤    │ ⊥       │
│ ⊥    │ ⊤       │
└──────┴─────────┘
```
"""
function not end
const ¬ = not

"""
    p ≺ q
    ≺(p, q)
    left(p, q)

```≺``` can be typed by ```\\prec<tab>```.

# Examples
```jldoctest
julia> @truth_table p ≺ q
┌──────┐
│ p    │
│ Atom │
├──────┤
│ ⊤    │
│ ⊥    │
└──────┘
```
"""
function left end
const ≺ = left

"""
    p ⊀ q
    ⊀(p, q)
    not_left(p, q)

```⊀``` can be typed by ```\\nprec<tab>```.

# Examples
```jldoctest
julia> @truth_table p ⊀ q
┌──────┬─────────┐
│ p    │ ¬p      │
│ Atom │ Literal │
├──────┼─────────┤
│ ⊤    │ ⊥       │
│ ⊥    │ ⊤       │
└──────┴─────────┘
```
"""
function not_left end
const ⊀ = not_left

"""
    p ≻ q
    ≻(p, q)
    right(p, q)

```≻``` can be typed by ```\\succ<tab>```.

# Examples
```jldoctest
julia> @truth_table p ≻ q
┌──────┐
│ q    │
│ Atom │
├──────┤
│ ⊤    │
│ ⊥    │
└──────┘
```
"""
function right end
const ≻ = right

"""
    p ⊁ q
    ⊁(p, q)
    left(p, q)

```⊁``` can be typed by ```\\nsucc<tab>```.

# Examples
```jldoctest
julia> @truth_table p ⊁ q
┌──────┬─────────┐
│ q    │ ¬q      │
│ Atom │ Literal │
├──────┼─────────┤
│ ⊤    │ ⊥       │
│ ⊥    │ ⊤       │
└──────┴─────────┘
```
"""
function not_right end
const ⊁ = not_right

"""
    p ∧ q
    ∧(p, q)
    and(p, q)

Logical [conjunction](https://en.wikipedia.org/wiki/Logical_conjunction) operator.

```∧``` can be typed by ```\\wedge<tab>```.

# Examples
```jldoctest
julia> @truth_table p ∧ q
┌──────┬──────┬───────┐
│ p    │ q    │ p ∧ q │
│ Atom │ Atom │ Tree  │
├──────┼──────┼───────┤
│ ⊤    │ ⊤    │ ⊤     │
│ ⊥    │ ⊤    │ ⊥     │
├──────┼──────┼───────┤
│ ⊤    │ ⊥    │ ⊥     │
│ ⊥    │ ⊥    │ ⊥     │
└──────┴──────┴───────┘
```
"""
function and end
const ∧ = and

"""
    p ⊼ q
    ⊼(p, q)
    nand(p, q)

Logical [non-conjunction](https://en.wikipedia.org/wiki/Sheffer_stroke) operator.

```⊼``` can be typed by ```\\nand<tab>```.

# Examples
```jldoctest
julia> @truth_table p ⊼ q
┌──────┬──────┬───────┐
│ p    │ q    │ p ⊼ q │
│ Atom │ Atom │ Tree  │
├──────┼──────┼───────┤
│ ⊤    │ ⊤    │ ⊥     │
│ ⊥    │ ⊤    │ ⊤     │
├──────┼──────┼───────┤
│ ⊤    │ ⊥    │ ⊤     │
│ ⊥    │ ⊥    │ ⊤     │
└──────┴──────┴───────┘
```
"""
function nand end

"""
    p ⊽ q
    ⊽(p, q)
    nor(p, q)

Logical [non-disjunction](https://en.wikipedia.org/wiki/Logical_NOR) operator.

```⊽``` can be typed by ```\\nor<tab>```.

# Examples
```jldoctest
julia> @truth_table p ⊽ q
┌──────┬──────┬───────┐
│ p    │ q    │ p ⊽ q │
│ Atom │ Atom │ Tree  │
├──────┼──────┼───────┤
│ ⊤    │ ⊤    │ ⊥     │
│ ⊥    │ ⊤    │ ⊥     │
├──────┼──────┼───────┤
│ ⊤    │ ⊥    │ ⊥     │
│ ⊥    │ ⊥    │ ⊤     │
└──────┴──────┴───────┘
```
"""
function nor end

"""
    p ∨ q
    ∨(p, q)
    or(p, q)

Logical [disjunction](https://en.wikipedia.org/wiki/Logical_disjunction) operator.

```∨``` can be typed by ```\\vee<tab>```.

# Examples
```jldoctest
julia> @truth_table p ∨ q
┌──────┬──────┬───────┐
│ p    │ q    │ p ∨ q │
│ Atom │ Atom │ Tree  │
├──────┼──────┼───────┤
│ ⊤    │ ⊤    │ ⊤     │
│ ⊥    │ ⊤    │ ⊤     │
├──────┼──────┼───────┤
│ ⊤    │ ⊥    │ ⊤     │
│ ⊥    │ ⊥    │ ⊥     │
└──────┴──────┴───────┘
```
"""
function or end
const ∨ = or

"""
    p ⊻ q
    ⊻(p, q)
    xor(p, q)

Logical [exclusive disjunction](https://en.wikipedia.org/wiki/Exclusive_or) operator.

```⊻``` can be typed by ```\\xor<tab>```.

# Examples
```jldoctest
julia> @truth_table p ⊻ q
┌──────┬──────┬───────┐
│ p    │ q    │ p ⊻ q │
│ Atom │ Atom │ Tree  │
├──────┼──────┼───────┤
│ ⊤    │ ⊤    │ ⊥     │
│ ⊥    │ ⊤    │ ⊤     │
├──────┼──────┼───────┤
│ ⊤    │ ⊥    │ ⊤     │
│ ⊥    │ ⊥    │ ⊥     │
└──────┴──────┴───────┘
```
"""
function xor end

"""
    p ↔ q
    ↔(p, q)
    xnor(p, q)

Logical [exclusive non-disjunction](https://en.wikipedia.org/wiki/XNOR_gate) and
[biconditional](https://en.wikipedia.org/wiki/Logical_biconditional) operator.

```↔``` can be typed by ```\\leftrightarrow<tab>```.

# Examples
```jldoctest
julia> @truth_table p ↔ q
┌──────┬──────┬───────┐
│ p    │ q    │ p ↔ q │
│ Atom │ Atom │ Tree  │
├──────┼──────┼───────┤
│ ⊤    │ ⊤    │ ⊤     │
│ ⊥    │ ⊤    │ ⊥     │
├──────┼──────┼───────┤
│ ⊤    │ ⊥    │ ⊥     │
│ ⊥    │ ⊥    │ ⊤     │
└──────┴──────┴───────┘
```
"""
function xnor end
const ↔ = xnor

"""
    p ↛ q
    ↛(p, q)
    not_imply(p, q)

Logical [non-implication](https://en.wikipedia.org/wiki/Material_nonimplication) operator.

```↛``` can be typed by ```\\nrightarrow<tab>```.

# Examples
```jldoctest
julia> @truth_table p ↛ q
┌──────┬──────┬───────┐
│ p    │ q    │ p ↛ q │
│ Atom │ Atom │ Tree  │
├──────┼──────┼───────┤
│ ⊤    │ ⊤    │ ⊥     │
│ ⊥    │ ⊤    │ ⊥     │
├──────┼──────┼───────┤
│ ⊤    │ ⊥    │ ⊤     │
│ ⊥    │ ⊥    │ ⊥     │
└──────┴──────┴───────┘
```
"""
function not_imply end
const ↛ = not_imply

"""
    p → q
    →(p, q)
    imply(p, q)

Logical [implication](https://en.wikipedia.org/wiki/Material_conditional) operator.

```→``` can be typed by ```\\rightarrow<tab>```.

# Examples
```jldoctest
julia> @truth_table p → q
┌──────┬──────┬───────┐
│ p    │ q    │ p → q │
│ Atom │ Atom │ Tree  │
├──────┼──────┼───────┤
│ ⊤    │ ⊤    │ ⊤     │
│ ⊥    │ ⊤    │ ⊤     │
├──────┼──────┼───────┤
│ ⊤    │ ⊥    │ ⊥     │
│ ⊥    │ ⊥    │ ⊤     │
└──────┴──────┴───────┘
```
"""
function imply end
const → = imply

"""
    p ↚ q
    ↚(p, q)
    not_converse_imply(p, q)

Logical [converse non-implication](https://en.wikipedia.org/wiki/Converse_nonimplication) operator.

```↚``` can be typed by ```\\nleftarrow<tab>```.

# Examples
```jldoctest
julia> @truth_table p ↚ q
┌──────┬──────┬───────┐
│ p    │ q    │ p ↚ q │
│ Atom │ Atom │ Tree  │
├──────┼──────┼───────┤
│ ⊤    │ ⊤    │ ⊥     │
│ ⊥    │ ⊤    │ ⊤     │
├──────┼──────┼───────┤
│ ⊤    │ ⊥    │ ⊥     │
│ ⊥    │ ⊥    │ ⊥     │
└──────┴──────┴───────┘
```
"""
function not_converse_imply end
const ↚ = not_converse_imply

"""
    p ← q
    ←(p, q)
    converse_imply(p, q)

Logical [converse implication](https://en.wikipedia.org/wiki/Converse_(logic)#Implicational_converse) operator.

```←``` can be typed by ```\\leftarrow<tab>```.

# Examples
```jldoctest
julia> @truth_table p ← q
┌──────┬──────┬───────┐
│ p    │ q    │ p ← q │
│ Atom │ Atom │ Tree  │
├──────┼──────┼───────┤
│ ⊤    │ ⊤    │ ⊤     │
│ ⊥    │ ⊤    │ ⊥     │
├──────┼──────┼───────┤
│ ⊤    │ ⊥    │ ⊤     │
│ ⊥    │ ⊥    │ ⊤     │
└──────┴──────┴───────┘
```
"""
function converse_imply end
const ← = converse_imply

"""
    AndOr
"""
const AndOr = Union{typeof(and), typeof(or)}

"""
    NullaryOperator
"""
const NullaryOperator = Union{typeof(tautology), typeof(contradiction)}

"""
    UnaryOperator
"""
const UnaryOperator = Union{typeof(identity), typeof(not)}

"""
    BinaryOperator
"""
const BinaryOperator = Union{
    typeof(left),
    typeof(not_left),
    typeof(right),
    typeof(not_right),
    typeof(and),
    typeof(nand),
    typeof(nor),
    typeof(or),
    typeof(xor),
    typeof(xnor),
    typeof(imply),
    typeof(not_imply),
    typeof(converse_imply),
    typeof(not_converse_imply),
}
# TODO: make traits?

"""
    BooleanOperator

A union of [`NullaryOperator`](@ref), [`UnaryOperator`](@ref), and [`BinaryOperator`](@ref).
"""
const BooleanOperator = Union{NullaryOperator, UnaryOperator, BinaryOperator}
