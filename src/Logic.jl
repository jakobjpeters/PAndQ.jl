
module Logic

import Base.length, Base.print, Base.show

include("core.jl")
include("set.jl")
include("propositional.jl")
include("first_order.jl")
include("modal.jl")
# include("minimize.jl")

export
    # core.jl
    @primitive, depth, length, valuate, # utility

    # set.jl
    ⨉, cartesian_product,

    # propositional logic
    ⊤, ⊥, Tautology, Contradiction, # concrete valuations
    ¬, ∧, ⊼, ⊽, ∨, ⊻, →, ←, ↔, # boolean operators
    not, and, nand, nor, or, xor, if_then, then_if, only_if, # boolean operator alias'
    Primitive, Proposition, # language

    # first_order
    ∀, ∃, ∄,

    # modal logic
    Modal, Know, Believe, # modal operators
    Agent, K # language

return nothing
end # module
