
module Logic

import Base.length, Base.print, Base.show

include("core.jl")
include("set.jl")
include("propositional.jl")
# include("first_order.jl")
include("modal.jl")
# include("minimize.jl")

export
    # core.jl
    @infix, length, depth, print, # utility
    Operator, Language,

    # set.jl
    ⨉,

    # propositional logic
    Valuation, ⊤, ⊥, # boolean primitives
    Boolean, Not, And, # functionally complete boolean operators
    ¬, ∧, ∨, →, ←, ↔, # boolean operators
    # Base.⊽, Base.⊼, Base.⊻,
    Proposition, P, # language
    truth_table,

    # modal logic
    Modal, Know, Believe, # modal operators
    Agent, K # language

return nothing
end # module
