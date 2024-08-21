
# [Operators](@id operators_operators)

Operators accept `Bool`s, [truth values](@ref nullary_operators), and [propositions](@ref propositions). However, boolean values cannot interoperate with truth values and propositions. Operations on boolean values is always eagerly evaluated, whereas operations on truth values and propositions is evaluated according to the operator's [`Evaluation`]() trait.

!!! info
    Operations on propositions that have each been [`normalize`](@ref)d eagerly evaluate to another normalized proposition.
    This behavior is likely to be removed in v0.4.

Typing symbols with tab completion is performed by typing `\`, followed by the given characters, and then the `[TAB]` key. For example, `⊤` is typed with `\top[TAB]`. See also [Tab Completion](https://docs.julialang.org/en/v1/stdlib/REPL/#Tab-completion) and [Unicode Input](https://docs.julialang.org/en/v1/manual/unicode-input/).

Operator associativity determines how operators with the same precedence group their operands. For example, `∧` is left associative. Therefore, `p ∧ q ∧ r` is equivalent to `(p ∧ q) ∧ r`. Operator precedence determines how expressions with distinct operators are grouped together. Higher precedence operators will group their operands before lower precedence operators. For example, `∧` has a higher precedence than `∨`. Therefore, `p ∨ q ∧ r` is equivalent to `p ∨ (q ∧ r)`, even though both operators are left associative. See also Julia's documentation on [Operator Precedence and Associativity](https://docs.julialang.org/en/v1/manual/mathematical-operations/#Operator-Precedence-and-Associativity).

!!! info
    This associativity is determined by Julia's parser and is distinct from the [`Associativity`]() trait used to specify the direction an operator [`fold`](@ref)s.

!!! info
    `==` has a precedence of 7, which is higher than that of several binary operators. For those cases, you may need to use parentheses. For example, `p → q == r` parses as `p → (q == r)` rather than `(p → q) == r`.

| Name                         | Symbol | Tab Completion    | Associativity | Precedence |
|:-----------------------------|:-------|:------------------|:--------------|:-----------|
| [`tautology`](@ref)          | `⊤`    | \\top             | none          | 0          |
| [`contradiction`](@ref)      | `⊥`    | \\bot             | none          | 0          |
| [`identical`](@ref)          | `𝒾`    | \\scri            | none          | 0          |
| [`not`](@ref)                | `¬`    | \\neg             | right         | 0          |
| [`and`](@ref)                | `∧`    | \\wedge           | left          | 12         |
| [`or`](@ref)                 | `∨`    | \\vee             | left          | 11         |
| [`imply`](@ref)              | `→`    | \\rightarrow      | right         | 4          |
| [`exclusive_or`](@ref)       | `↮`    | \\nleftrightarrow | right         | 4          |
| [`converse_imply`](@ref)     | `←`    | \\leftarrow       | right         | 4          |
| [`not_and`](@ref)            | `↑`    | \\uparrow         | right         | 15         |
| [`not_or`](@ref)             | `↓`    | \\downarrow       | right         | 15         |
| [`not_imply`](@ref)          | `↛`    | \\nrightarrow     | right         | 4          |
| [`not_exclusive_or`](@ref)   | `↔`    | \\leftrightarrow  | right         | 4          |
| [`not_converse_imply`](@ref) | `↚`    | \\nleftarrow      | right         | 4          |
| [`conjunction`](@ref)        | `⋀`    | \\bigwedge        | none          | 0          |
| [`disjunction`](@ref)        | `⋁`    | \\bigvee          | none          | 0          |

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
