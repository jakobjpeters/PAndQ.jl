
import Base: nand, ⊼, nor, ⊽, xor, ⊻, mapfoldl, mapfoldr

# Nullary Operators

"""
    tautology()
    ⊤()

Logical [true](https://en.wikipedia.org/wiki/Tautology_(logic)) operator.

`⊤` can be typed by `\\top<tab>`.

# Examples
```jldoctest
julia> TruthTable([Tree(⊤)])
┌──────┐
│ ⊤    │
│ Tree │
├──────┤
│ ⊤    │
└──────┘
```
"""
function tautology end
const ⊤ = tautology

"""
    contradiction()
    ⊥()

Logical [false](https://en.wikipedia.org/wiki/Contradiction) operator.

`⊥` can be typed by `\\bot<tab>`.

# Examples
```jldoctest
julia> TruthTable([Tree(⊥)])
┌──────┐
│ ⊥    │
│ Tree │
├──────┤
│ ⊥    │
└──────┘
```
"""
function contradiction end
const ⊥ = contradiction

# Unary Operators

"""
    identity(p)

Logical [identity](https://en.wikipedia.org/wiki/Law_of_identity) operator.

# Examples
```jldoctest
julia> @atomize TruthTable([identity(p)])
┌──────────┐
│ p        │
│ Variable │
├──────────┤
│ ⊤        │
│ ⊥        │
└──────────┘
```
"""
identity

"""
    not(p)
    ¬p

Logical [negation](https://en.wikipedia.org/wiki/Negation) operator.

`¬` can be typed by `\\neg<tab>`.

# Examples
```jldoctest
julia> @atomize TruthTable([¬p])
┌──────────┬─────────┐
│ p        │ ¬p      │
│ Variable │ Literal │
├──────────┼─────────┤
│ ⊤        │ ⊥       │
│ ⊥        │ ⊤       │
└──────────┴─────────┘
```
"""
function not end
const ¬ = not

# Binary Operators

"""
    and(p, q)
    p ∧ q

Logical [conjunction](https://en.wikipedia.org/wiki/Logical_conjunction) operator.

`∧` can be typed by `\\wedge<tab>`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ∧ q])
┌──────────┬──────────┬───────┐
│ p        │ q        │ p ∧ q │
│ Variable │ Variable │ Tree  │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊤        │ ⊤     │
│ ⊥        │ ⊤        │ ⊥     │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊥        │ ⊥     │
│ ⊥        │ ⊥        │ ⊥     │
└──────────┴──────────┴───────┘
```
"""
function and end
const ∧ = and

"""
    nand(p, q)
    p ⊼ q

Logical [non-conjunction](https://en.wikipedia.org/wiki/Sheffer_stroke) operator.

`⊼` can be typed by `\\nand<tab>`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ⊼ q])
┌──────────┬──────────┬───────┐
│ p        │ q        │ p ⊼ q │
│ Variable │ Variable │ Tree  │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊤        │ ⊥     │
│ ⊥        │ ⊤        │ ⊤     │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊥        │ ⊤     │
│ ⊥        │ ⊥        │ ⊤     │
└──────────┴──────────┴───────┘
```
"""
nand

"""
    nor(p, q)
    p ⊽ q

Logical [non-disjunction](https://en.wikipedia.org/wiki/Logical_NOR) operator.

`⊽` can be typed by `\\nor<tab>`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ⊽ q])
┌──────────┬──────────┬───────┐
│ p        │ q        │ p ⊽ q │
│ Variable │ Variable │ Tree  │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊤        │ ⊥     │
│ ⊥        │ ⊤        │ ⊥     │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊥        │ ⊥     │
│ ⊥        │ ⊥        │ ⊤     │
└──────────┴──────────┴───────┘
```
"""
nor

"""
    or(p, q)
    p ∨ q

Logical [disjunction](https://en.wikipedia.org/wiki/Logical_disjunction) operator.

`∨` can be typed by `\\vee<tab>`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ∨ q])
┌──────────┬──────────┬───────┐
│ p        │ q        │ p ∨ q │
│ Variable │ Variable │ Tree  │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊤        │ ⊤     │
│ ⊥        │ ⊤        │ ⊤     │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊥        │ ⊤     │
│ ⊥        │ ⊥        │ ⊥     │
└──────────┴──────────┴───────┘
```
"""
function or end
const ∨ = or

"""
    xor(p, q)
    p ⊻ q

Logical [exclusive disjunction](https://en.wikipedia.org/wiki/Exclusive_or) operator.

`⊻` can be typed by `\\xor<tab>`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ⊻ q])
┌──────────┬──────────┬───────┐
│ p        │ q        │ p ⊻ q │
│ Variable │ Variable │ Tree  │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊤        │ ⊥     │
│ ⊥        │ ⊤        │ ⊤     │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊥        │ ⊤     │
│ ⊥        │ ⊥        │ ⊥     │
└──────────┴──────────┴───────┘
```
"""
xor

"""
    xnor(p, q)
    p ↔ q

Logical [exclusive non-disjunction](https://en.wikipedia.org/wiki/XNOR_gate)
and [biconditional](https://en.wikipedia.org/wiki/Logical_biconditional) operator.

`↔` can be typed by `\\leftrightarrow<tab>`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ↔ q])
┌──────────┬──────────┬───────┐
│ p        │ q        │ p ↔ q │
│ Variable │ Variable │ Tree  │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊤        │ ⊤     │
│ ⊥        │ ⊤        │ ⊥     │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊥        │ ⊥     │
│ ⊥        │ ⊥        │ ⊤     │
└──────────┴──────────┴───────┘
```
"""
function xnor end
const ↔ = xnor

"""
    not_imply(p, q)
    p ↛ q

Logical [non-implication](https://en.wikipedia.org/wiki/Material_nonimplication) operator.

`↛` can be typed by `\\nrightarrow<tab>`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ↛ q])
┌──────────┬──────────┬───────┐
│ p        │ q        │ p ↛ q │
│ Variable │ Variable │ Tree  │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊤        │ ⊥     │
│ ⊥        │ ⊤        │ ⊥     │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊥        │ ⊤     │
│ ⊥        │ ⊥        │ ⊥     │
└──────────┴──────────┴───────┘
```
"""
function not_imply end
const ↛ = not_imply

"""
    imply(p, q)
    p → q

Logical [implication](https://en.wikipedia.org/wiki/Material_conditional) operator.

`→` can be typed by `\\rightarrow<tab>`.

# Examples
```jldoctest
julia> @atomize TruthTable([p → q])
┌──────────┬──────────┬───────┐
│ p        │ q        │ p → q │
│ Variable │ Variable │ Tree  │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊤        │ ⊤     │
│ ⊥        │ ⊤        │ ⊤     │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊥        │ ⊥     │
│ ⊥        │ ⊥        │ ⊤     │
└──────────┴──────────┴───────┘
```
"""
function imply end
const → = imply

"""
    not_converse_imply(p, q)
    p ↚ q

Logical [converse non-implication](https://en.wikipedia.org/wiki/Converse_nonimplication) operator.

`↚` can be typed by `\\nleftarrow<tab>`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ↚ q])
┌──────────┬──────────┬───────┐
│ p        │ q        │ p ↚ q │
│ Variable │ Variable │ Tree  │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊤        │ ⊥     │
│ ⊥        │ ⊤        │ ⊤     │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊥        │ ⊥     │
│ ⊥        │ ⊥        │ ⊥     │
└──────────┴──────────┴───────┘
```
"""
function not_converse_imply end
const ↚ = not_converse_imply

"""
    converse_imply(p, q)
    p ← q

Logical [converse implication](https://en.wikipedia.org/wiki/Converse_(logic)#Implicational_converse) operator.

`←` can be typed by `\\leftarrow<tab>`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ← q])
┌──────────┬──────────┬───────┐
│ p        │ q        │ p ← q │
│ Variable │ Variable │ Tree  │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊤        │ ⊤     │
│ ⊥        │ ⊤        │ ⊥     │
├──────────┼──────────┼───────┤
│ ⊤        │ ⊥        │ ⊤     │
│ ⊥        │ ⊥        │ ⊤     │
└──────────┴──────────┴───────┘
```
"""
function converse_imply end
const ← = converse_imply

# Internals

## Union Types

"""
    NullaryOperator

The `Union` of [`LogicalOperator`](@ref)s that take zero arguments.
"""
const NullaryOperator = union_typeof((tautology, contradiction))

"""
    UnaryOperator

The `Union` of [`LogicalOperator`](@ref)s that take one argument.
"""
const UnaryOperator = union_typeof((identity, not))

"""
    BinaryOperator

The `Union` of [`LogicalOperator`](@ref)s that take two arguments.
"""
const BinaryOperator = union_typeof((
    and, nand, nor, or, xor, xnor, imply, not_imply, converse_imply, not_converse_imply
))

"""
    LogicalOperator

The `Union` of [logical operators](@ref operators_operators).
"""
const LogicalOperator = Union{NullaryOperator, UnaryOperator, BinaryOperator}

"""
    CommutativeOperator

The `Union` of [`LogicalOperator`](@ref)s with the [commutative property]
(https://en.wikipedia.org/wiki/Commutative_property).
"""
const CommutativeOperator = union_typeof((and, nand, nor, or, xor, xnor))

"""
    AssociativeOperator

The `Union` of [`LogicalOperator`](@ref)s with the [associative property]
(https://en.wikipedia.org/wiki/Associative_property).
"""
const AssociativeOperator = union_typeof((and, or, xor, xnor))

"""
    LeftNeutralOperator

The `Union` of [`LogicalOperator`](@ref)s that have one or more [`left_neutrals`](@ref).
"""
const LeftNeutralOperator = Union{
    AssociativeOperator,
    union_typeof((imply, not_converse_imply))
}

"""
    RightNeutralOperator

The `Union` of [`LogicalOperator`](@ref)s that have one or more [`right_neutrals`](@ref).
"""
const RightNeutralOperator = Union{
    AssociativeOperator,
    union_typeof((not_imply, converse_imply))
}

"""
    AndOr

The `Union` of [`and`](@ref) and [`or`](@ref).
"""
const AndOr = union_typeof((and, or))

# Reductions

"""
    conjunction(ps)
    ⋀(ps)

Equivalent to `foldl(and, ps; init = tautology)`.

`⋀` can be typed by `\\bigwedge<tab>`.

See also [`and`](@ref) and [`tautology`](@ref).

# Examples
```jldoctest
julia> @atomize ⋀([p, q, r, s])
((p ∧ q) ∧ r) ∧ s
```
"""
conjunction(ps) = foldl(and, ps)
const ⋀ = conjunction

"""
    disjunction(ps)
    ⋁(ps)

Equivalent to `foldl(or, ps; init = contradiction)`.

`⋁` can be typed by `\\bigvee<tab>`.

See also [`or`](@ref) and [`contradiction`](@ref).

# Examples
```jldoctest
julia> @atomize ⋁([p, q, r, s])
((p ∨ q) ∨ r) ∨ s
```
"""
disjunction(ps) = foldl(or, ps)
const ⋁ = disjunction

"""
    mapfoldl(f, lio::LeftNeutralOperator, ps)

Equivalent to `mapfoldl(f, lio, ps; init = only(left_neutrals(lio)))`.

See also [`LeftNeutralOperator`](@ref).

!!! tip
    This also works with `foldl(lio, ps)`.

# Examples
```jldoctest
julia> @atomize mapfoldl(not, and, [p, q, r, s])
((¬p ∧ ¬q) ∧ ¬r) ∧ ¬s

julia> foldl(and, [])
tautology (generic function with 1 method)
```
"""
mapfoldl(f, lio::LeftNeutralOperator, ps) =
    mapfoldl(f, lio, ps, init = only(left_neutrals(lio)))

"""
    mapfoldr(f, rio::RightNeutralOperator, ps)

Equivalent to `mapfoldr(f, rio, ps; init = only(right_neutrals(rio)))`.

See also [`RightNeutralOperator`](@ref).

!!! tip
    This also works with `foldr(rio, ps)`.

# Examples
```jldoctest
julia> @atomize mapfoldr(not, and, [p, q, r, s])
¬p ∧ (¬q ∧ (¬r ∧ ¬s))

julia> foldr(and, [])
tautology (generic function with 1 method)
```
"""
mapfoldr(f, rio::RightNeutralOperator, ps) =
    mapfoldr(f, rio, ps, init = only(right_neutrals(rio)))

# Utilities

"""
    arity(::LogicalOperator)

Return the [arity](https://en.wikipedia.org/wiki/Arity)
of the given [`LogicalOperator`](@ref).

# Examples
```jldoctest
julia> arity(tautology)
0

julia> arity(not)
1

julia> arity(and)
2
```
"""
arity(::NullaryOperator) = 0
arity(::UnaryOperator) = 1
arity(::BinaryOperator) = 2
