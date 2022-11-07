
using PAQ

function main()
    boolean_operators = [¬, ∧, ⊼, ⊽, ∨, ⊻, →, ←, ↔]

    p = Primitive("Modal logic is hard")
    q = Primitive("This code has bugs")
    r = p → q

    for f in boolean_operators
        if f == ¬
            println(f(p))
        else
            println(f(p, q))
        end
        println()
    end

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
