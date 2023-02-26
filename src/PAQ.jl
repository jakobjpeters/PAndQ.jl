
# TODO: module docstring?
module PAQ

include("operators.jl")
include("types.jl")
include("pretty_printing.jl")
include("utility.jl")
include("semantics.jl")

export
    # operators.jl
    #= Base.identity, =# not, ¬, left, ≺, not_left, ⊀, right, ≻, not_right, ⊁, and, ∧,
    #= Base.nand, Base.:⊼, Base.nor, Base.:⊽, =# or, ∨, #= Base.xor, Base.:⊻, =#
    xnor, ↔, imply, →, not_imply, ↛, converse_imply, ←, not_converse_imply, ↚,

    # types.jl
    Proposition, Primitive, Compound, Expressive,
    Truth, tautology, ⊤, contradiction, ⊥,
    AndOr, NullaryOperator, UnaryOperator, BinaryOperator, BooleanOperator,
    Atom, Literal, Clause, Normal, Interpretation, Valuation, Tree,

    # pretty_printing.jl
    Pretty, @pretty, #= AbstractTrees.children, AbstractTrees.nodevalue, =#
    print_tree, truth_table, @truth_table, #= Base.show, =#

    # utility.jl
    @atoms, @p, @p_str, get_atoms,
    #= Base,foldl, Base.foldr, Base.reduce, =#

    # semantics.jl
    interpret, #= Base.:==, =#
    is_tautology, is_contradiction, is_truth, is_contingency, is_satisfiable, is_falsifiable,
    is_commutative, is_associative, converse, dual, solve #= Base.identity, =#
end # module
