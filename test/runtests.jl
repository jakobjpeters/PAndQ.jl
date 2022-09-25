
using Logic

boolean_operators = [¬, ∧, ∨, →, ←, ↔]

for f in boolean_operators
    (f == ¬) && continue
    println("Truth table of: ", f)
    println(truth_table(f))
    println()
end

p = ∧(⊤, ⊥)
println(p)
println("⊤ ∧ ⊥ ≡ ", p())
