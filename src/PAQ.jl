
module PAQ

# include("core.jl")
include("set.jl")
include("propositional.jl")
include("first_order.jl")
include("modal.jl")
include("minimize.jl")
include("utility.jl")
include("pretty.jl")

export
    # core.jl
    @primitive, depth, length, valuate, # utility

    # set.jl
    ⨉, cartesian_product,

    # propositional logic
    Valuation, ⊤, ⊥, Tautology, Contradiction, # valuations
    Operator, Not, And, # functionally complete operations
    ¬, ∧, ⊼, ⊽, ∨, ⊻, →, ←, ↔, # boolean operators
    not, and, nand, nor, or, xor, if_then, then_if, only_if, # boolean operator alias'
    Language, Compound, Primitive, Propositional, # propositions

    # first_order
    ∀, ∃, ∄,

    # modal logic
    temp_modal, Know, Believe, # modal operators
    Agent, K # language

return nothing
end # module
