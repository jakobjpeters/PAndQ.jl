
using Logic

function main()
    boolean_operators = [¬, ∧, ⊼, ⊽, ∨, ⊻, →, ←, ↔]

    for f in boolean_operators
        println("Truth table of: ", f)
        println(truth_table(f))
        println()
    end

    p = Proposition("Modal logic is hard")
    q = ¬Proposition("This code has bugs")
    r = p → q

    println("r ≡ ", r())
    println("⊤ ∨ s ≡ ", (⊤ ∨ r)())
    println()

    println("length(s): ", length(r))
    println("depth(s): ", depth(r))
end

main()