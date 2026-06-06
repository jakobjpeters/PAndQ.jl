
# Nullary Operators

"""
    tautology()
    ⊤()

Logical [true](https://en.wikipedia.org/wiki/Tautology_(logic)) operator.

`⊤` can be typed by `\\top[TAB]`.

# Examples
```jldoctest
julia> print_table(⊤)
┌───┐
│ ⊤ │
├───┤
│ ⊤ │
└───┘
```
"""
const tautology = ⊤ = operator(:tautology)

"""
    contradiction()
    ⊥()

Logical [false](https://en.wikipedia.org/wiki/Contradiction) operator.

`⊥` can be typed by `\\bot[TAB]`.

# Examples
```jldoctest
julia> print_table(⊥)
┌───┐
│ ⊥ │
├───┤
│ ⊥ │
└───┘
```
"""
const contradiction = ⊥ = operator(:contradiction)

# Unary Operators

"""
    identical(p)
    𝒾(p)

Logical [identity](https://en.wikipedia.org/wiki/Law_of_identity) operator.

# Examples
```jldoctest
julia> @atomize print_table(𝒾(p))
┌───┐
│ p │
├───┤
│ ⊤ │
│ ⊥ │
└───┘
```
"""
const identical = 𝒾 = operator(:identical)

"""
    not(p)
    ¬p

Logical [negation](https://en.wikipedia.org/wiki/Negation) operator.

`¬` can be typed by `\\neg[TAB]`.

# Examples
```jldoctest
julia> @atomize print_table(¬p)
┌───┬────┐
│ p │ ¬p │
├───┼────┤
│ ⊤ │ ⊥  │
│ ⊥ │ ⊤  │
└───┴────┘
```
"""
const not = ¬ = operator(:not)

# Binary Operators

"""
    and(p, q)
    p ∧ q

Logical [conjunction](https://en.wikipedia.org/wiki/Logical_conjunction) operator.

`∧` can be typed by `\\wedge[TAB]`.

# Examples
```jldoctest
julia> @atomize print_table(p ∧ q)
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
const and = ∧ = operator(:and)

"""
    or(p, q)
    p ∨ q

Logical [disjunction](https://en.wikipedia.org/wiki/Logical_disjunction) operator.

`∨` can be typed by `\\vee[TAB]`.

# Examples
```jldoctest
julia> @atomize print_table(p ∨ q)
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
const or = ∨ = operator(:or)

"""
    imply(p, q)
    p → q

Logical [implication](https://en.wikipedia.org/wiki/Material_conditional) operator.

`→` can be typed by `\\rightarrow[TAB]`.

# Examples
```jldoctest
julia> @atomize print_table(p → q)
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
const imply = → = operator(:imply)

"""
    exclusive_or(p, q)
    p ↮ q

Logical [exclusive disjunction](https://en.wikipedia.org/wiki/Exclusive_or) operator.

`↮` can be typed by `\\nleftrightarrow[TAB]`.

# Examples
```jldoctest
julia> @atomize print_table(p ↮ q)
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
const exclusive_or = ↮ = operator(:exclusive_or)

"""
    converse_imply(p, q)
    p ← q

Logical [converse implication](https://en.wikipedia.org/wiki/Converse_(logic)#Implicational_converse) operator.

`←` can be typed by `\\leftarrow[TAB]`.

# Examples
```jldoctest
julia> @atomize print_table(p ← q)
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
const converse_imply = ← = operator(:converse_imply)

"""
    not_and(p, q)
    p ↑ q

Logical [non-conjunction](https://en.wikipedia.org/wiki/Sheffer_stroke) operator.

`↑` can be typed by `\\uparrow[TAB]`.

# Examples
```jldoctest
julia> @atomize print_table(p ↑ q)
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
const not_and = ↑ = operator(:not_and)

"""
    not_or(p, q)
    p ↓ q

Logical [non-disjunction](https://en.wikipedia.org/wiki/Logical_NOR) operator.

`↓` can be typed by `\\downarrow[TAB]`.

# Examples
```jldoctest
julia> @atomize print_table(p ↓ q)
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
const not_or = ↓ = operator(:not_or)

"""
    not_exclusive_or(p, q)
    p ↔ q

Logical [exclusive non-disjunction]
(https://en.wikipedia.org/wiki/Logical_biconditional) operator.

`↔` can be typed by `\\leftrightarrow[TAB]`.

# Examples
```jldoctest
julia> @atomize print_table(p ↔ q)
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
const not_exclusive_or = ↔ = operator(:not_exclusive_or)

"""
    not_imply(p, q)
    p ↛ q

Logical [non-implication](https://en.wikipedia.org/wiki/Material_nonimplication) operator.

`↛` can be typed by `\\nrightarrow[TAB]`.

# Examples
```jldoctest
julia> @atomize print_table(p ↛ q)
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
const not_imply = ↛ = operator(:not_imply)

"""
    not_converse_imply(p, q)
    p ↚ q

Logical [converse non-implication](https://en.wikipedia.org/wiki/Converse_nonimplication) operator.

`↚` can be typed by `\\nleftarrow[TAB]`.

# Examples
```jldoctest
julia> @atomize print_table(p ↚ q)
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
const not_converse_imply = ↚ = operator(:not_converse_imply)

# Nary Operators

"""
    conjunction(ps...)
    ⋀(ps...)

Equivalent to `fold(𝒾, (∧) => ps)`.

`⋀` can be typed by `\\bigwedge[TAB]`.

See also [`identical`](@ref), [`and`](@ref), and [`fold`](@ref).

# Examples
```jldoctest
julia> ⋀()
⊤

julia> @atomize ⋀(p, q, r, s)
p ∧ q ∧ r ∧ s
```
"""
const conjunction = ⋀ = operator(:conjunction)

"""
    disjunction(ps...)
    ⋁(ps...)

Equivalent to `fold(𝒾, (∨) => ps)`.

`⋁` can be typed by `\\bigvee[TAB]`.

See also [`identical`](@ref), [`or`](@ref), and [`fold`](@ref).

# Examples
```jldoctest
julia> ⋁()
⊥

julia> @atomize ⋁(p, q, r, s)
p ∨ q ∨ r ∨ s
```
"""
const disjunction = ⋁ = operator(:disjunction)

# Utilities

____fold(associativity) = associativity == left ? mapfoldl : mapfoldr

___fold(mapfold, f, o, xs, ::Nothing) = mapfold(f, o, xs)
___fold(mapfold, f, o, xs, initial_value) =
    isempty(xs) ? AbstractSyntaxTree(initial_value) : mapfold(f, o, xs)

__fold(f, o, xs) = g -> (args...) -> ___fold(
    ____fold(associativities[o]), x -> f(g)(args..., x),
o, xs, initial_value(o))

_fold() = identity
_fold((o, xs)) = __fold(identity, o, xs)
_fold((o, xs), pairs...) = __fold(_fold(pairs...), o, xs)

"""
    fold(f, pairs...)

A generalization of `mapreduce` with an arbitrary number of nested folds
and traits to determine each operator's
[`Associativity`](@ref Interface.Associativity) and [`initial_value`](@ref Interface.initial_value).

The function `f` must accept as many parameters as there are `pairs`.
Each pair must be a two element iterable where the first element is a
binary operator and the second element is an iterable to be folded over.

Given a single pair, this function is similar to `mapreduce` and other similar functions.
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
fold(a => xs, b => ys, ...) do x, y, ...
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
fold(f, pairs...) = _fold(pairs...)(f)()
