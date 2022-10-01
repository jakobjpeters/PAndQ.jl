
using Logic

function main()
    boolean_operators = [¬, ∧, ⊼, ⊽, ∨, ⊻, →, ←, ↔]

    for f in boolean_operators
        println("Truth table of: ", f)
        println(truth_table(f))
        println()
    end

    p = Primitive("Modal logic is hard")
    q = Primitive("This code has bugs")
    r = p → q

    println("r ≡ ", r())
    println("⊤ ∨ s ≡ ", (⊤ ∨ r)())
    println()

    println("length(s): ", length(r))
    println("depth(s): ", depth(r))

    for valuation in [⊤, ⊥]
        println(valuation)
        println(valuation())
        println(valuation()())
        println(valuation()()())
        println()
    end

    for xy in ⨉([⊤, ⊥, p], [⊥, ⊤, q])
        x, y = xy
        println(x, y)
        r = x ∧ y
        println(r)
        println(r())
        println()
    end

    states = Dict{Primitive, Type}(p => ⊤, q => ⊤)

end

main()
