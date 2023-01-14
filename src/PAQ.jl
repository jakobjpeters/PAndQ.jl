
module PAQ

include("set.jl")
include("propositional_logic.jl")
include("pretty_printing.jl")
include("semantics.jl")
include("boolean_operators.jl")
# include("predicate_logic.jl")

export
    # set.jl
    ⨉, cartesian_product,

    # propositional_logic.jl
    Proposition, Compound, Atom, Literal, Tree, Normal,
    Truth, tautology, ⊤, contradiction, ⊥, Contingency,
    @atom, get_atoms, #= Base.convert, =#

    # boolean_operators.jl
    not, ¬, and, ∧, #= Base.nand, Base.:⊼, Base.nor, Base.:⊽, =# or, ∨,
    #= Base.xor, Base.:⊻, =# xnor, ↔, if_then, →, not_if_then, ↛, then_if, ←, not_then_if, ↚,

    # semantics.jl
    interpret, #= Base.:==, =#
    is_tautology, is_contradiction, is_truth, is_contingency, is_satisfiable, is_falsifiable,

    # pretty_printing.jl
    #=Base.repr, Base.show, =# print_tree, Pretty, @pretty, @truth_table

return nothing

end # module
