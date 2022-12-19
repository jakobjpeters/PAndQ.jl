
module PAQ

using Combinatorics

include("set.jl")
include("abstract_types.jl")
include("propositional_logic.jl")
include("pretty_printing.jl")
include("semantics.jl")
include("boolean_operators.jl")
# include("predicate_logic.jl")

export
    # set.jl
    ⨉, cartesian_product,

    # abstract_types.jl
    Language, Compound, Operator,

    # propositional_logic.jl
    Boolean, Not, And, Or,
    Primitive, Literal, Propositional, Normal,
    Truth, tautology, ⊤, contradiction, ⊥, Contingency,
    @primitive, get_primitives, #= Base.convert, =#

    # boolean_operators.jl
    not, ¬, and, ∧, #= Base.nand, Base.:⊼, Base.nor, Base.:⊽, =# or, ∨,
    #= Base.xor, Base.:⊻, =# xnor, ↔, if_then, →, not_if_then, ↛, then_if, ←, not_then_if, ↚,

    # semantics.jl
    interpret, #= Base.:==, =#
    is_tautology, is_contradiction, is_truth, is_contingency, is_satisfiable, is_falsifiable,
    @truth_table,

    # pretty_printing.jl
    Show, @Show #=, Base.show =#

return nothing

end # module
