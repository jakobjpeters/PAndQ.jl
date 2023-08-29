
# TODO: module docstring?
module PAndQ

"""
    union_typeof
"""
union_typeof(xs) = Union{map(typeof, xs)...}

include("operators.jl")

export
    tautology, ⊤,
    contradiction, ⊥,
    #= Base.identity =#
    not, ¬,
    and, ∧,
    #= Base.nand, Base.:⊼ =#
    #= Base.nor, Base.:⊽ =#
    or, ∨,
    #= Base.xor, Base.:⊻ =#
    xnor, ↔,
    imply, →,
    not_imply, ↛,
    converse_imply, ←,
    not_converse_imply, ↚,
    conjunction, ⋀,
    disjunction, ⋁,
    #= Base.mapfoldl, Base.mapfoldr =#
    arity

include("propositions.jl")

export
    Proposition, Compound, Expressive,
    Atom, Literal, Tree, Clause, Normal,
    @atoms, @atomize, @p_str,
    atoms, operators

include("printing.jl")

export
    TruthTable,
    #= Base.show =#
    #= AbstractTrees =# print_tree,
    print_truth_table,
    println_truth_table,
    print_latex,
    println_latex,
    print_markdown,
    println_markdown

include("semantics.jl")

export
    dual, converse,
    left_neutrals, right_neutrals,
    valuations, interpret, interpretations, solve,
    ==,
    is_tautology, is_contradiction,
    is_truth, is_contingency,
    is_satisfiable, is_falsifiable
    #= Base.convert =#

end # module
