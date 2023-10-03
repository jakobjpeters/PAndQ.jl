
```@meta
DocTestSetup = :(using PAndQ)
```

# [Operators](@id operators_operators)

Typing symbols with tab completion is performed by typing `\`, followed by the given characters, and then the `[TAB]` key. For example, `⊤` is typed with `\top[TAB]`. See also Julia's documentation on [Tab Completion](https://docs.julialang.org/en/v1/stdlib/REPL/#Tab-completion) and [Unicode Input](https://docs.julialang.org/en/v1/manual/unicode-input/).

Operator associativity determines how operators with the same precedence group their operands. For example, `∧` is left associative. Therefore, `p ∧ q ∧ r` is equivalent to `(p ∧ q) ∧ r`. Operator precedence determines how expressions with distinct operators are grouped together. Higher precedence operators will group their operands before lower precedence operators. For example, `∧` has a higher precedence than `∨`. Therefore, `p ∨ q ∧ r` is equivalent to `p ∨ (q ∧ r)`, even though both operators are left associative. See also Julia's documentation on [Operator Precedence and Associativity](https://docs.julialang.org/en/v1/manual/mathematical-operations/#Operator-Precedence-and-Associativity).

!!! info
    `==` has a precedence of 7, which is higher than that of several binary operators. For those cases, you may need to use parentheses. For example, `@atomize p → q == r` parses as `@atomize p → (q == r)` rather than `@atomize (p → q) == r`.

| Name                         | Symbol | Tab Completion   | Associativity | Precedence |
|:-----------------------------|:-------|:-----------------|:--------------|:-----------|
| [`tautology`](@ref)          | `⊤`    | \\top            | none          | 0          |
| [`contradiction`](@ref)      | `⊥`    | \\bot            | none          | 0          |
| [`identity`](@ref)           | `𝒾`    | \\scri           | none          | 0          |
| [`not`](@ref !)              | `¬`    | \\neg            | right         | 0          |
| [`and`](@ref &)              | `∧`    | \\wedge          | left          | 12         |
| [`nand`](@ref)               | `⊼`    | \\nand           | left          | 12         |
| [`nor`](@ref)                | `⊽`    | \\nor            | left          | 11         |
| [`or`](@ref |)               | `∨`    | \\vee            | left          | 11         |
| [`xor`](@ref)                | `⊻`    | \\xor            | left          | 11         |
| [`xnor`](@ref)               | `↔`    | \\leftrightarrow | right         | 4          |
| [`imply`](@ref)              | `→`    | \\rightarrow     | right         | 4          |
| [`not_imply`](@ref)          | `↛`    | \\nrightarrow    | right         | 4          |
| [`converse_imply`](@ref)     | `←`    | \\leftarrow      | right         | 4          |
| [`not_converse_imply`](@ref) | `↚`    | \\nleftarrow     | right         | 4          |

## Nullary Operators

```@docs
tautology
contradiction
```

## Unary Operators

```@docs
identity
!
```

## Binary Operators

!!! tip
    Each binary operator `bo` has been curried such that
    `bo(p) = q -> bo(p, q)` and `bo(p)(q) == bo(p, q)`.

```@docs
&
nand
nor
|
xor
xnor
imply
not_imply
converse_imply
not_converse_imply
```

## Reductions

```@docs
conjunction
disjunction
```

## Utilities

```@docs
arity
```