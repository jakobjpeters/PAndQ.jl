
"""
    Operator{N}
"""
struct Operator{N} end

# Nullary Operators

"""
    tautology()
    ⊤()

Logical [true](https://en.wikipedia.org/wiki/Tautology_(logic)) operator.

`⊤` can be typed by `\\top[TAB]`.

# Examples
```jldoctest
julia> TruthTable([⊤])
┌───┐
│ ⊤ │
├───┤
│ ⊤ │
└───┘
```
"""
const tautology = ⊤ = Operator{:tautology}()

"""
    contradiction()
    ⊥()

Logical [false](https://en.wikipedia.org/wiki/Contradiction) operator.

`⊥` can be typed by `\\bot[TAB]`.

# Examples
```jldoctest
julia> TruthTable([⊥])
┌───┐
│ ⊥ │
├───┤
│ ⊥ │
└───┘
```
"""
const contradiction = ⊥ = Operator{:contradiction}()

# Unary Operators

"""
    identical(p)
    𝒾(p)

Logical [identity](https://en.wikipedia.org/wiki/Law_of_identity) operator.

# Examples
```jldoctest
julia> @atomize TruthTable([𝒾(p)])
┌───┐
│ p │
├───┤
│ ⊤ │
│ ⊥ │
└───┘
```
"""
const identical = 𝒾 = Operator{:identical}()

"""
    not(p)
    ¬p

Logical [negation](https://en.wikipedia.org/wiki/Negation) operator.

`¬` can be typed by `\\neg[TAB]`.

# Examples
```jldoctest
julia> @atomize TruthTable([¬p])
┌───┬────┐
│ p │ ¬p │
├───┼────┤
│ ⊤ │ ⊥  │
│ ⊥ │ ⊤  │
└───┴────┘
```
"""
const not = ¬ = Operator{:not}()

# Binary Operators

"""
    and(p, q)
    p ∧ q

Logical [conjunction](https://en.wikipedia.org/wiki/Logical_conjunction) operator.

`∧` can be typed by `\\wedge[TAB]`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ∧ q])
┌───┬───┬───────┐
│ p │ q │ p ∧ q │
├───┼───┼───────┤
│ ⊤ │ ⊤ │ ⊤     │
│ ⊥ │ ⊤ │ ⊥     │
├───┼───┼───────┤
│ ⊤ │ ⊥ │ ⊥     │
│ ⊥ │ ⊥ │ ⊥     │
└───┴───┴───────┘
```
"""
const and = ∧ = Operator{:and}()

"""
    or(p, q)
    p ∨ q

Logical [disjunction](https://en.wikipedia.org/wiki/Logical_disjunction) operator.

`∨` can be typed by `\\vee[TAB]`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ∨ q])
┌───┬───┬───────┐
│ p │ q │ p ∨ q │
├───┼───┼───────┤
│ ⊤ │ ⊤ │ ⊤     │
│ ⊥ │ ⊤ │ ⊤     │
├───┼───┼───────┤
│ ⊤ │ ⊥ │ ⊤     │
│ ⊥ │ ⊥ │ ⊥     │
└───┴───┴───────┘
```
"""
const or = ∨ = Operator{:or}()

"""
    imply(p, q)
    p → q

Logical [implication](https://en.wikipedia.org/wiki/Material_conditional) operator.

`→` can be typed by `\\rightarrow[TAB]`.

# Examples
```jldoctest
julia> @atomize TruthTable([p → q])
┌───┬───┬───────┐
│ p │ q │ p → q │
├───┼───┼───────┤
│ ⊤ │ ⊤ │ ⊤     │
│ ⊥ │ ⊤ │ ⊤     │
├───┼───┼───────┤
│ ⊤ │ ⊥ │ ⊥     │
│ ⊥ │ ⊥ │ ⊤     │
└───┴───┴───────┘
```
"""
const imply = → = Operator{:imply}()

"""
    exclusive_or(p, q)
    p ↮ q

Logical [exclusive disjunction](https://en.wikipedia.org/wiki/Exclusive_or) operator.

`↮` can be typed by `\\nleftrightarrow[TAB]`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ↮ q])
┌───┬───┬───────┐
│ p │ q │ p ↮ q │
├───┼───┼───────┤
│ ⊤ │ ⊤ │ ⊥     │
│ ⊥ │ ⊤ │ ⊤     │
├───┼───┼───────┤
│ ⊤ │ ⊥ │ ⊤     │
│ ⊥ │ ⊥ │ ⊥     │
└───┴───┴───────┘
```
"""
const exclusive_or = ↮ = Operator{:exclusive_or}()

"""
    converse_imply(p, q)
    p ← q

Logical [converse implication](https://en.wikipedia.org/wiki/Converse_(logic)#Implicational_converse) operator.

`←` can be typed by `\\leftarrow[TAB]`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ← q])
┌───┬───┬───────┐
│ p │ q │ p ← q │
├───┼───┼───────┤
│ ⊤ │ ⊤ │ ⊤     │
│ ⊥ │ ⊤ │ ⊥     │
├───┼───┼───────┤
│ ⊤ │ ⊥ │ ⊤     │
│ ⊥ │ ⊥ │ ⊤     │
└───┴───┴───────┘
```
"""
const converse_imply = ← = Operator{:converse_imply}()

"""
    not_and(p, q)
    p ↑ q

Logical [non-conjunction](https://en.wikipedia.org/wiki/Sheffer_stroke) operator.

`↑` can be typed by `\\uparrow[TAB]`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ↑ q])
┌───┬───┬───────┐
│ p │ q │ p ↑ q │
├───┼───┼───────┤
│ ⊤ │ ⊤ │ ⊥     │
│ ⊥ │ ⊤ │ ⊤     │
├───┼───┼───────┤
│ ⊤ │ ⊥ │ ⊤     │
│ ⊥ │ ⊥ │ ⊤     │
└───┴───┴───────┘
```
"""
const not_and = ↑ = Operator{:not_and}()

"""
    not_or(p, q)
    p ↓ q

Logical [non-disjunction](https://en.wikipedia.org/wiki/Logical_NOR) operator.

`↓` can be typed by `\\downarrow[TAB]`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ↓ q])
┌───┬───┬───────┐
│ p │ q │ p ↓ q │
├───┼───┼───────┤
│ ⊤ │ ⊤ │ ⊥     │
│ ⊥ │ ⊤ │ ⊥     │
├───┼───┼───────┤
│ ⊤ │ ⊥ │ ⊥     │
│ ⊥ │ ⊥ │ ⊤     │
└───┴───┴───────┘
```
"""
const not_or = ↓ = Operator{:not_or}()

"""
    not_exclusive_or(p, q)
    p ↔ q

Logical [exclusive non-disjunction]
(https://en.wikipedia.org/wiki/Logical_biconditional) operator.

`↔` can be typed by `\\leftrightarrow[TAB]`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ↔ q])
┌───┬───┬───────┐
│ p │ q │ p ↔ q │
├───┼───┼───────┤
│ ⊤ │ ⊤ │ ⊤     │
│ ⊥ │ ⊤ │ ⊥     │
├───┼───┼───────┤
│ ⊤ │ ⊥ │ ⊥     │
│ ⊥ │ ⊥ │ ⊤     │
└───┴───┴───────┘
```
"""
const not_exclusive_or = ↔ = Operator{:not_exclusive_or}()

"""
    not_imply(p, q)
    p ↛ q

Logical [non-implication](https://en.wikipedia.org/wiki/Material_nonimplication) operator.

`↛` can be typed by `\\nrightarrow[TAB]`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ↛ q])
┌───┬───┬───────┐
│ p │ q │ p ↛ q │
├───┼───┼───────┤
│ ⊤ │ ⊤ │ ⊥     │
│ ⊥ │ ⊤ │ ⊥     │
├───┼───┼───────┤
│ ⊤ │ ⊥ │ ⊤     │
│ ⊥ │ ⊥ │ ⊥     │
└───┴───┴───────┘
```
"""
const not_imply = ↛ = Operator{:not_imply}()

"""
    not_converse_imply(p, q)
    p ↚ q

Logical [converse non-implication](https://en.wikipedia.org/wiki/Converse_nonimplication) operator.

`↚` can be typed by `\\nleftarrow[TAB]`.

# Examples
```jldoctest
julia> @atomize TruthTable([p ↚ q])
┌───┬───┬───────┐
│ p │ q │ p ↚ q │
├───┼───┼───────┤
│ ⊤ │ ⊤ │ ⊥     │
│ ⊥ │ ⊤ │ ⊤     │
├───┼───┼───────┤
│ ⊤ │ ⊥ │ ⊥     │
│ ⊥ │ ⊥ │ ⊥     │
└───┴───┴───────┘
```
"""
const not_converse_imply = ↚ = Operator{:not_converse_imply}()

# Nary Operators

"""
    conjunction(ps)
    ⋀(ps)

Equivalent to `something(foldl(∧, ps; init = Some(⊤)))`.

`⋀` can be typed by `\\bigwedge[TAB]`.

See also [`and`](@ref) and [`tautology`](@ref).

# Examples
```jldoctest
julia> @atomize ⋀((p, q, r, s))
((p ∧ q) ∧ r) ∧ s
```
"""
conjunction(ps) = fold(𝒾, (∧) => ps)
const ⋀ = conjunction

"""
    disjunction(ps)
    ⋁(ps)

Equivalent to `something(foldl(∨, ps; init = Some(⊥)))`.

`⋁` can be typed by `\\bigvee[TAB]`.

See also [`or`](@ref) and [`contradiction`](@ref).

# Examples
```jldoctest
julia> @atomize ⋁((p, q, r, s))
((p ∨ q) ∨ r) ∨ s
```
"""
disjunction(ps) = fold(𝒾, (∨) => ps)
const ⋁ = disjunction

# Internals

"""
    FoldDirection(::Operator)

A trait to indicate which direction to fold a binary operator.

Supertype of [`Left`](@ref) and [`Right`](@ref).
See also [`Operator`](@ref).

# Examples
```jldoctest
julia> PAndQ.FoldDirection(→)
PAndQ.Left()

julia> PAndQ.FoldDirection(←)
PAndQ.Right()
```
"""
abstract type FoldDirection end

"""
    Left <: FoldDirection

A trait to indicate that a binary operator should fold left.

Subtype of [`FoldDirection`](@ref).
"""
struct Left <: FoldDirection end
FoldDirection(::union_typeof((∧, ↑, ↓, ∨, ↮, ↔, →, ↚))) = Left()

"""
    Right <: FoldDirection

A trait to indicate that a binary operator should fold right.

Subtype of [`FoldDirection`](@ref).
"""
struct Right <: FoldDirection end
FoldDirection(::union_typeof((↛, ←))) = Right()

"""
    InitialValue(::Operator)

A trait to indicate whether a binary operator has an initial value.

Supertype of [`HasInitialValue`](@ref) and [`NoInitialValue`](@ref).
See also [`Operator`](@ref).

# Examples
```jldoctest
julia> PAndQ.InitialValue(∧)
PAndQ.HasInitialValue()

julia> PAndQ.InitialValue(↑)
PAndQ.NoInitialValue()
```
"""
abstract type InitialValue end

"""
    HasInitialValue <: InitialValue

A trait to indicate that a binary operator has an initial value.

Subtype of [`InitialValue`](@ref).
"""
struct HasInitialValue <: InitialValue end
InitialValue(::union_typeof((∧, ∨, ↮, ↔, →, ↛, ←, ↚))) = HasInitialValue()

"""
    NoInitialValue <: InitialValue

A trait to indicate that a binary operator does not have a neutral element.

Subtype of [`InitialValue`](@ref).
"""
struct NoInitialValue <: InitialValue end
InitialValue(::union_typeof((↑, ↓))) = NoInitialValue()

"""
    initial_value(::Operator)

See also [`Operator`](@ref).

# Examples
```jldoctest
julia> PAndQ.initial_value(∧)
⊤

julia> PAndQ.initial_value(∨)
⊥
```
"""
initial_value(::union_typeof((∧, ↔, →, ←))) = ⊤
initial_value(::union_typeof((∨, ↮, ↚, ↛))) = ⊥

## Union Types

"""
    NullaryOperator

The `Union` of [Nullary Operators](@ref nullary_operators).
"""
const NullaryOperator = union_typeof((⊤, ⊥))

"""
    UnaryOperator

The `Union` of [Unary Operators](@ref unary_operators).
"""
const UnaryOperator = union_typeof((𝒾, ¬))

"""
    BinaryOperator

The `Union` of [Binary Operators](@ref binary_operators).
"""
const BinaryOperator = union_typeof((∧, ↑, ↓, ∨, ↮, ↔, →, ↛, ←, ↚))

"""
    NaryOperator

The `Union` of [Nary Operators](@ref nary_operators).
"""
const NaryOperator = union_typeof((⋀, ⋁))

"""
    AndOr

The `Union` of [`and`](@ref) and [`or`](@ref).
"""
const AndOr = union_typeof((∧, ∨))

# Utilities

____fold(::Left) = mapfoldl
____fold(::Right) = mapfoldr

___fold(::NoInitialValue, mapfold, f, operator, xs) = mapfold(f, operator, xs)
function ___fold(::HasInitialValue, mapfold, f, operator, xs)
    _initial_value = initial_value(operator)
    isempty(xs) ? _initial_value : mapfold(f, operator, xs)
end

__fold(f, operator, xs) = g -> (args...) -> ___fold(InitialValue(operator),
    ____fold(FoldDirection(operator)), x -> f(g)(args..., x), operator, xs)

_fold() = 𝒾
_fold((operator, xs)) = __fold(𝒾, operator, xs)
_fold((operator, xs), pairs...) = __fold(_fold(pairs...), operator, xs)

"""
    fold(f, pairs...)

A generalization of `mapreduce` with an arbitrary number of nested folds
and traits to determine the [`FoldDirection`](@ref) and [`InitialValue`](@ref).

The function `f` must accept as many arguments as there are `pairs`.
Each pair must be an two element iterable where the first element is a
binary operator and the second element is an iterable.

Given a single pair, this function is similar to `mapreduce` and other related functions.
Giving additional pairs will generalize the following pattern:

```julia
mapreduce(a, xs) do x
    mapreduce(b, ys) do y
        ...
    end
end
```

This can be rewritten as:

```julia
fold(a => xs, b => ys, ...) do (x, y, ...)
    ...
end
```

# Examples
```jldoctest
julia> fold(⊤)
⊤

julia> @atomize fold(¬, (∧) => (p, q))
¬p ∧ ¬q

julia> @atomize fold(↔, (∧) => (p, q), (∨) => (r, s))
((p ↔ r) ∨ (p ↔ s)) ∧ ((q ↔ r) ∨ (q ↔ s))
```
"""
fold(f::Union{Function, Operator}, pairs::Pair...) = _fold(pairs...)(f)()
fold(pair) = fold(𝒾, pair)

"""
    arity(operator)

Return the [arity](https://en.wikipedia.org/wiki/Arity)
of the given [operator](@ref operators_operators).

# Examples
```jldoctest
julia> arity(⊤)
0

julia> arity(¬)
1

julia> arity(∧)
2

julia> arity(⋀)
Inf
```
"""
arity(::NullaryOperator) = 0
arity(::UnaryOperator) = 1
arity(::BinaryOperator) = 2
arity(::NaryOperator) = Inf
