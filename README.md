

## About

This project implements logics (currently only propositional logic, but with more to come) written from scratch.


## Examples

``` julia
using Logic

p = Proposition("Modal logic is hard")
q = ¬(Proposition("This code has bugs"))
r = →(p, q)

r()
# ⊥

∨(⊤, r)()
# ⊤

∧(p, q)
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
¬(p, q)    # not p                    \neg
∧(p, q)    # p and q                  \wedge
∨(p, q)    # p or q                   \vee
→(p, q)    # p implies q              \rightarrow
←(p, q)    # q implies p              \leftarrow
↔(p, q)    # p implies q implies p    \leftrightarrow

⨉(A, B)    # cartesian product        \bigtimes
⊤          # tautology                \top
⊥          # contradiction            \bot

@infix f = ϕ      # syntactic sugar for formulae - only partially implemented
truth_table(f)    # vector of all input => output for boolean operation f
length(p)         # total number of operators and propositions in a formula
depth(p)          # maximum nested modal operators
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
