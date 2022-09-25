

## About

This project implements logics (currently only propositional logic, but with more to come) written from scratch.


## Examples

``` julia
using Logic

truth_table(↔)
#=
2×2 Matrix{Pair{Tuple{DataType, DataType}, DataType}}:
 (⊤, ⊥)=>⊥  (⊤, ⊤)=>⊤
 (⊥, ⊥)=>⊤  (⊥, ⊤)=>⊥
=#

∧(⊤, ⊥)
#=
Language(
  And(), Language(
    Proposition{Nothing}
  ) Language(
    Not(), Language(
      Proposition{Nothing}
    ) 
  ) 
) 
=#

∧(⊤, ⊥)()
#=
⊥
=#
```


## Known Bugs

- Printing in the REPL does not work yet, use a script instead


## To Do

- Implement
    - Sets
    - First order logic
    - Modal logic


## References

Van Ditmarsch, Hans, et al. Handbook of epistemic logic. College Publications, 2015.
