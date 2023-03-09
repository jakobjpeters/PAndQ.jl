
# TODO: module docstring?
module PAQ

include("operators.jl")
include("propositions.jl")
include("printing.jl")
include("utility.jl")
include("semantics.jl")

export
    # operators.jl
    tautology, ⊤, contradiction, ⊥,
    #= Base.identity, =# not, ¬,
    and, ∧, #= Base.nand, Base.:⊼, Base.nor, Base.:⊽, =# or, ∨, #= Base.xor, Base.:⊻, =#
    xnor, ↔, imply, →, not_imply, ↛, converse_imply, ←, not_converse_imply, ↚,
    #= NullaryOperator, UnaryOperator, BinaryOperator, BooleanOperator,
    CommutativeOperator, AssociativeOperator, AndOr, =#

    # types.jl
    Proposition, Compound, Expressive,
    Atom, Literal, Clause, Normal, Valuation, Tree,

    # pretty_printing.jl
    #= AbstractTrees.children, AbstractTrees.nodevalue, =#
    print_tree, truth_table, @truth_table, #= Base.show, Base.print, =#

    # utility.jl
    @atoms, @p, @p_str, get_atoms, #= Base.mapfoldl, Base.mapfoldr, Base.mapreduce =#

    # semantics.jl
    interpret, #= Base.:==, =#
    is_tautology, is_contradiction, is_truth, is_contingency, is_satisfiable, is_falsifiable,
    converse, dual, solve #= Base.identity, =#

end # module
