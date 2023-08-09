
# TODO: module docstring?
module PAndQ

include("operators.jl")
include("propositions.jl")
include("printing.jl")
include("utility.jl")
include("semantics.jl")

export
    # operators.jl
    tautology, ⊤, contradiction, ⊥,
    #= Base.identity, =# not, ¬,
    and, ∧, #= Base.nand, Base.:⊼, Base.nor, Base.:⊽, =#
    or, ∨, #= Base.xor, Base.:⊻, =# xnor, ↔, imply, →,
    not_imply, ↛, converse_imply, ←, not_converse_imply, ↚,

    # propositions.jl
    Proposition, Compound, Expressive,
    Atom, Literal, Tree, Clause, Normal,

    # printing.jl
    TruthTable, print_truth_table, println_truth_table,
    print_tree, println_tree, print_latex, println_latex,
    #= Markdown. =# MD, print_markdown, println_markdown,
    #= Base.show, Base.print, =#

    # utility.jl
    arity, @atoms, @p, @p_str, atoms, eltype,
    conjunction, ⋀, disjunction, ⋁, #= Base.mapfoldl, Base.mapfoldr, =#
    random_proposition,

    # semantics.jl
    #= Base.==, =# interpret, interpretations, valuations, solve,
    left_neutrals, right_neutrals, converse, dual,
    is_tautology, is_contradiction, is_truth, is_contingency, is_satisfiable, is_falsifiable
    #= , Base.Bool, Base.convert =#

end # module
