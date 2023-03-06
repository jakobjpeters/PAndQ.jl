
```@meta
DocTestSetup = :(using PAQ)
```

# Boolean Operators

Every possible truth table can be constructed with the functionally complete set of operators ```not``` and ```and```. For convenience, all sixteen of them have been prepared. There are ten binary operators, with the remaining six being expressed with truth values, individual propositions, and the unary [`not`](@ref) operator.

```jldoctest
julia> @truth_table ⊤ ⊥ ¬p ¬q
┌────────┬────────┬──────┬──────┬─────────┬─────────┐
│ ⊤      │ ⊥      │ p    │ q    │ ¬p      │ ¬q      │
│ Clause │ Clause │ Atom │ Atom │ Literal │ Literal │
├────────┼────────┼──────┼──────┼─────────┼─────────┤
│ ⊤      │ ⊥      │ ⊤    │ ⊤    │ ⊥       │ ⊥       │
│ ⊤      │ ⊥      │ ⊥    │ ⊤    │ ⊤       │ ⊥       │
├────────┼────────┼──────┼──────┼─────────┼─────────┤
│ ⊤      │ ⊥      │ ⊤    │ ⊥    │ ⊥       │ ⊤       │
│ ⊤      │ ⊥      │ ⊥    │ ⊥    │ ⊤       │ ⊤       │
└────────┴────────┴──────┴──────┴─────────┴─────────┘
```

Typing symbols with tab completion is performed by typing ```\```, followed by the given characters, and then the ```[TAB]``` key. See also Julia's documentation on [Tab Completion](https://docs.julialang.org/en/v1/stdlib/REPL/#Tab-completion) and [Unicode Input](https://docs.julialang.org/en/v1/manual/unicode-input/). Operator associativity determines how operators with the same precedence group their operands. For example, ```∧``` is left associative. Therefore, ```p ∧ q ∧ r``` is equivalent to ```(p ∧ q) ∧ r```. Operator precedence determines how expressions with distinct operators are grouped together. Higher precedence operators will group their operands before lower precedence operators. For example, ```∧``` has a higher precedence than ```∨```. Therefore, ```p ∨ q ∧ r``` is equivalent to ```p ∨ (q ∧ r)```, even though both operators are left associative. See also Julia's documentation on [Operator Precedence and Associativity](https://docs.julialang.org/en/v1/manual/mathematical-operations/#Operator-Precedence-and-Associativity).

| Name                         | Symbol | Tab completion   | Associativity | Precedence |
|:-----------------------------|:-------|:-----------------|:--------------|:-----------|
| [`tautology`](@ref)          | ⊤      | \\top            | none?         | 0          |
| [`contradiction`](@ref)      | ⊥      | \\bot            | none?         | 0          |
| [`not`](@ref)                | ¬      | \\neg            | right         | 4          |
| [`and`](@ref)                | ∧      | \\wedge          | left          | 3          |
| [`nand`](@ref)               | ⊼      | \\nand           | left          | 3          |
| [`nor`](@ref)                | ⊽      | \\nor            | left          | 2          |
| [`or`](@ref)                 | ∨      | \\vee            | left          | 2          |
| [`xor`](@ref)                | ⊻      | \\xor            | left          | 2          |
| [`xnor`](@ref)               | ↔      | \\leftrightarrow | right         | 1          |
| [`imply`](@ref)              | →      | \\rightarrow     | right         | 1          |
| [`not_imply`](@ref)          | ↛      | \\nrightarrow    | right         | 1          |
| [`converse_imply`](@ref)     | ←      | \\leftarrow      | right         | 1          |
| [`not_converse_imply`](@ref) | ↚      | \\nleftarrow     | right         | 1          |

```@docs
tautology
contradiction
identity
not
and
nand
nor
or
xor
xnor
imply
not_imply
converse_imply
not_converse_imply
AndOr
NullaryOperator
UnaryOperator
BinaryOperator
BooleanOperator
```
