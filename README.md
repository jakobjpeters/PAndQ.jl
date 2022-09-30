

## About

This project implements logics (currently only propositional logic, but with more to come) written from scratch.


## Examples

``` julia
using Logic

p = Proposition("Modal logic is hard")
q = ¬Proposition("This code has bugs")
r = p → q

r()
# ⊥

(⊤ ∨ r)()
# ⊤

p ∧ q
#=
Language(
  And(), Language(
    Proposition{String}("Modal logic is hard")
  ) Language(
    Not(), Language(
      Proposition{String}("This code has bugs")
    ) 
  ) 
) 
=#

⊥()
# ⊥()

⊥()()
#=
Language(
  Not(), Language(
    Proposition{Nothing}(nothing)
  ) 
)
=#

⊥()()()
# ⊥

truth_table(↔)
#=
2×2 Matrix{Pair{Tuple{DataType, DataType}, DataType}}:
 (⊤, ⊥)=>⊥  (⊤, ⊤)=>⊤
 (⊥, ⊥)=>⊤  (⊥, ⊤)=>⊥
=#
```


## Functions

``` julia
¬(p, q)    # not           \neg
∧(p, q)    # and           \wedge
⊼(p, q)    # Logic.nand    \nand
⊽(p, q)    # Logic.nor     \nor
∨(p, q)    # or            \vee
⊻(p, q)    # Logic.xor     \veebar
→(p, q)    # imply_r       \rightarrow
←(p, q)    # imply_l       \leftarrow
↔(p, q)    # imply_lr      \leftrightarrow

⨉(A, B)    # cartesian product    \bigtimes
⊤          # tautology            \top
⊥          # contradiction        \bot

@infix f = ϕ      # syntactic sugar - doesn't do anything yet
truth_table(f)    # vector of all input => output for boolean operation f
length(p)         # total number of operators and propositions in a formula
depth(p)          # maximum nested modal operators
```


## Known Bugs

- @inline
    - doesn't do anything (yet)
    - doesn't work (yet)


## To Do

- Implement
    - Sets
    - First order logic
    - Second order logic
    - Higher order logic
    - Modal logic


## References

Van Ditmarsch, Hans, et al. Handbook of epistemic logic. College Publications, 2015.
