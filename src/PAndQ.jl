
module PAndQ

using PrecompileTools: @compile_workload

"""
    union_typeof(xs)
"""
union_typeof(xs) = Union{map(typeof, xs)...}

include("operators.jl")

export
    tautology, ⊤,
    contradiction, ⊥,
    𝒾,
    not, ¬,
    and, ∧,
    or, ∨,
    xnor, ↔,
    imply, →,
    not_imply, ↛,
    converse_imply, ←,
    not_converse_imply, ↚,
    conjunction, ⋀,
    disjunction, ⋁,
    arity, map_reducers

include("propositions.jl")

export
    @atomize, @variables, constants,
    value, atoms, operators,
    normalize, tseytin

include("semantics.jl")

export
    valuations, interpret, interpretations, solutions,
    is_commutative, is_associative,
    is_tautology, is_contradiction,
    is_truth, is_contingency,
    is_satisfiable, is_falsifiable,
    is_equisatisfiable, ==,
    dual, converse,
    left_neutrals, right_neutrals

include("printing.jl")

export
    TruthTable,
    formatter,
    pretty_table,
    print_tree

__init__() = @compile_workload begin
    @variables p q

    ps = (⊤, ⊥, p, ¬p, map(BO -> BO.instance(p, q), uniontypes(BinaryOperator))...)
    qs = (ps..., conjunction(ps), disjunction(ps))

    pretty_table(String, TruthTable(qs))

    for r in qs
        interpret(p => ⊤, r)
        interpret(p => ⊥, r)

        for iterator in map(f -> f(r), (atoms, operators, solutions))
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
