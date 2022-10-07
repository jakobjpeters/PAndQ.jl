
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
    @infix, @primitive, depth, length, # utility
    Operator, Language,

    # set.jl
    ⨉, cartesian_product,

    # propositional logic
    Valuation, ⊤, ⊥, tautology, contradiction, # boolean primitives
    Boolean, Not, And, # functionally complete boolean operators
    ¬, ∧, ⊼, ⊽, ∨, ⊻, →, ←, ↔, # boolean operator symbols
    not, and, nand, nor, or, xor, imply_r, imply_l, imply_lr, # boolean operator words
    Primitive, PL, # language

    # first_order
    ∀, ∃, ∄,

    # modal logic
    Modal, Know, Believe, # modal operators
    Agent, K # language

return nothing
end # module
