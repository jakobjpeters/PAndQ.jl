
module PAndQ

using PrecompileTools: @compile_workload

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
    Proposition, Atom, Compound, Expressive,
    Constant, Variable, Literal, Tree, Clause, Normal,
    @atomize, @variables,
    atoms, operators
    #= Base.map =#

include("printing.jl")

export
    TruthTable,
    #= Base.show =#
    formatter,
    #= PrettyTables =# pretty_table,
    #= AbstractTrees =# print_tree

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

@compile_workload let
    @variables p q

    ps = [Tree(⊤), Tree(⊥), p, ¬p, map(BO -> BO.instance(p, q), uniontypes(BinaryOperator))...]
    qs = [ps..., conjunction(ps), disjunction(ps)]

    pretty_table(String, TruthTable(qs))

    for r in qs
        r(p => true)
        r(p => ⊤)
        r(p => ⊥)

        for iterator in map(f -> f(r), (atoms, operators, solve))
            collect(iterator)
        end

        for f in (
            is_tautology, is_contradiction,
            is_truth, is_contingency,
            is_satisfiable, is_falsifiable
        )
            f(r)
        end

        for args in ((show,), (show, MIME"text/plain"()), (pretty_table,), (print_tree,))
            sprint(args..., r)
        end
    end
end

end # module
