
module PAQ

using Combinatorics

include("set.jl")
include("abstract_types.jl")
include("propositional_logic.jl")
include("boolean_operators.jl")
# include("predicate_logic.jl")
include("semantics.jl")
include("pretty_printing.jl")

export
    # set.jl
    ⨉, cartesian_product,

    # abstract_types.jl
    Language, Compound, Operator,

    # propositional_logic.jl
    Primitive, Boolean, Not, And, Propositional,
    Truth, tautology, ⊤, contradiction, ⊥, 
    @primitive, get_primitives,

    # boolean_operators.jl
    not, and, or, xnor, if_then, not_if_then, then_if, not_then_if,
    ¬, ∧, ∨, ↔, →, ↛, ←, ↚,
    # nand, ⊼, nor, ⊽, xor, ⊻ - exported by Base

    # semantics.jl
    is_tautology, is_contradiction, is_contingency, is_satisfiable, is_falsifiable,
    ==, @truth_table

    # pretty_printing.jl
    # print, show - exported by Base

return nothing

end # module
