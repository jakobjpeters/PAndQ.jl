
```@meta
DocTestSetup = quote
    using PAQ
    @atom p q
end
```

# Boolean Operators

Every possible truth table can be constructed with the functionally complete set of operators
```not``` and ```and```. For convenience, all sixteen of them have been prepared.
There are ten binary operators, with the remaining six being expressed with
individual propositions, the unary [`not`](@ref) operator, and [`Truth`](@ref) constants.

```jldoctest
julia> @truth_table ¬p ¬q ⊤ ⊥
┌──────┬──────┬─────────┬─────────┬───────┬───────┐
│ p    │ q    │ ¬p      │ ¬q      │ ⊤     │ ⊥     │
│ Atom │ Atom │ Literal │ Literal │ Truth │ Truth │
│ "p"  │ "q"  │         │         │       │       │
├──────┼──────┼─────────┼─────────┼───────┼───────┤
│ ⊤    │ ⊤    │ ⊥       │ ⊥       │ ⊤     │ ⊥     │
│ ⊤    │ ⊥    │ ⊥       │ ⊤       │ ⊤     │ ⊥     │
├──────┼──────┼─────────┼─────────┼───────┼───────┤
│ ⊥    │ ⊤    │ ⊤       │ ⊥       │ ⊤     │ ⊥     │
│ ⊥    │ ⊥    │ ⊤       │ ⊤       │ ⊤     │ ⊥     │
└──────┴──────┴─────────┴─────────┴───────┴───────┘
```

| Name                   | Symbol | Tab completion   |
|:-----------------------|:-------|:-----------------|
| [`not`](@ref)          | ¬      | \\neg            |
| [`and`](@ref)          | ∧      | \\wedge          |
| [`nand`](@ref)         | ⊼      | \\nand           |
| [`nor`](@ref)          | ⊽      | \\nor            |
| [`or`](@ref)           | ∨      | \\vee            |
| [`xor`](@ref)          | ⊻      | \\xor            |
| [`xnor`](@ref)         | ↔      | \\leftrightarrow |
| [`if_then`](@ref)      | →      | \\rightarrow     |
| [`not_if_then`](@ref)  | ↛      | \\nrightarrow    |
| [`then_if`](@ref)      | ←      | \\leftarrow      |
| [`not_then_if`](@ref)  | ↚      | \\nleftarrow     |

```@docs
not
and
nand
nor
or
xor
xnor
if_then
not_if_then
then_if
not_then_if
```
