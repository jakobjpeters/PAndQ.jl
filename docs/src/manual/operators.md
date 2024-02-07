
# [Operators](@id operators_operators)

Operators accept `Bool`s, [Nullary Operators](@ref nullary_operators), and [`Proposition`](@ref PAndQ.Proposition)s. Operations on symbolic expressions are not simplified.

Typing symbols with tab completion is performed by typing `\`, followed by the given characters, and then the `[TAB]` key. For example, `⊤` is typed with `\top[TAB]`. See also [Tab Completion](https://docs.julialang.org/en/v1/stdlib/REPL/#Tab-completion) and [Unicode Input](https://docs.julialang.org/en/v1/manual/unicode-input/).

Operator associativity determines how operators with the same precedence group their operands. For example, `∧` is left associative. Therefore, `p ∧ q ∧ r` is equivalent to `(p ∧ q) ∧ r`. Operator precedence determines how expressions with distinct operators are grouped together. Higher precedence operators will group their operands before lower precedence operators. For example, `∧` has a higher precedence than `∨`. Therefore, `p ∨ q ∧ r` is equivalent to `p ∨ (q ∧ r)`, even though both operators are left associative. See also Julia's documentation on [Operator Precedence and Associativity](https://docs.julialang.org/en/v1/manual/mathematical-operations/#Operator-Precedence-and-Associativity).

!!! info
    `==` has a precedence of 7, which is higher than that of several binary operators. For those cases, you may need to use parentheses. For example, `@atomize p → q == r` parses as `@atomize p → (q == r)` rather than `@atomize (p → q) == r`.

| Name                         | Symbol | Tab Completion    |
|:-----------------------------|:-------|:------------------|
| [`tautology`](@ref)          | `⊤`    | \\top             |
| [`contradiction`](@ref)      | `⊥`    | \\bot             |
| [`identical`](@ref)          | `𝒾`    | \\scri            |
| [`not`](@ref)                | `¬`    | \\neg             |
| [`and`](@ref)                | `∧`    | \\wedge           |
| [`or`](@ref)                 | `∨`    | \\vee             |
| [`imply`](@ref)              | `→`    | \\rightarrow      |
| [`exclusive_or`](@ref)       | `↮`    | \\nleftrightarrow |
| [`converse_imply`](@ref)     | `←`    | \\leftarrow       |
| [`not_and`](@ref)            | `↑`    | \\uparrow         |
| [`not_or`](@ref)             | `↓`    | \\downarrow       |
| [`not_imply`](@ref)          | `↛`    | \\nrightarrow     |
| [`not_exclusive_or`](@ref)   | `↔`    | \\leftrightarrow  |
| [`not_converse_imply`](@ref) | `↚`    | \\nleftarrow      |
| [`conjunction`](@ref)        | `⋀`    | \\bigwedge        |
| [`disjunction`](@ref)        | `⋁`    | \\bigvee          |

## [Nullary Operators](@id nullary_operators)

```@docs
tautology
contradiction
```

## [Unary Operators](@id unary_operators)

```@docs
identical
not
```

## [Binary Operators](@id binary_operators)

!!! tip
    Each binary operator `bo` has been [curried](https://en.wikipedia.org/wiki/Currying)
    such that `bo(p) = Base.Fix2(bo, p)` and `bo(p)(q) == bo(q, p)`.

```@docs
and
or
imply
exclusive_or
converse_imply
not_and
not_or
not_imply
not_exclusive_or
not_converse_imply
```

## [Nary Operators](@id nary_operators)

```@docs
conjunction
disjunction
```

## Utilities

```@docs
fold
```