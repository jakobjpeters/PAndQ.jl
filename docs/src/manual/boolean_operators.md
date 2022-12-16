
# Boolean Operators

| Name                   | Symbol | Tab completion   |
|:-----------------------|:-------|:-----------------|
| [`not`](@ref)          | ¬      | \\neg            |
| [`and`](@ref)          | ∧      | \\wedge          |
| [`nand`](@ref)         | ∧      | \\nand           |
| [`nor`](@ref)          | ∨      | \\nor            |
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
nand(::Language, ::Language)
nor(::Language, ::Language)
or
xor
xnor
if_then
not_if_then
then_if
not_then_if
```
