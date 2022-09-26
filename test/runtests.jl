
using Logic

function main()
    boolean_operators = [¬, ∧, ⊼, ⊽, ∨, ⊻, →, ←, ↔]

    for f in boolean_operators
        println("Truth table of: ", f)
        println(truth_table(f))
        println()
    end

    p = Proposition("Propositional logic is fun")
    q = Proposition("Modal logic is hard")
    r = ¬Proposition("This code has bugs")
    s = q → r

    println(q ∧ r)
    println()

    println("s ≡ ", s())
    println("p ∨ s ≡ ", (p ∨ s)())
    println()

    println("length(s): ", length(s))
    println("depth(s): ", depth(s))
end

main()