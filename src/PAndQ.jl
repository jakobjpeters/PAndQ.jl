
module PAndQ

using PrecompileTools: @compile_workload

"""
    union_typeof(xs)

Equivalent to `Union{map(typeof, xs)...}`.
"""
union_typeof(xs) = Union{map(typeof, xs)...}

include("solvers/PicoSAT.jl")

export PicoSAT

include("solvers/Z3.jl")

export Z3

include("interface.jl")

import .Interface:
    Associativity,
    arity, dual, evaluate, initial_value,
    is_associative, is_commutative, print_expression, print_proposition, symbol
using .Interface: left, Operator, right, parenthesize
export Interface

include("operators.jl")

export
    tautology, ⊤,
    contradiction, ⊥,
    identical, 𝒾,
    not, ¬,
    and, ∧,
    or, ∨,
    imply, →,
    exclusive_or, ↮,
    converse_imply, ←,
    not_and, ↑,
    not_or, ↓,
    not_imply, ↛,
    not_exclusive_or, ↔,
    not_converse_imply, ↚,
    conjunction, ⋀,
    disjunction, ⋁,
    fold

include("propositions.jl")

export
    @atomize, @variables, constants,
    value, atoms, install_atomize_mode,
    normalize, tseytin

include("semantics.jl")

export
    valuations, interpret, interpretations, solutions,
    is_tautology, is_contradiction,
    is_truth, is_contingency,
    is_satisfiable, is_falsifiable,
    is_equisatisfiable, is_equivalent

include("printing.jl")

export
    TruthTable,
    formatter,
    print_proposition,
    print_table,
    print_tree,
    print_dimacs

# @compile_workload for (p, q) in (@atomize([$:p, $:q]), @variables p q) redirect_stdout(devnull) do
#     rs = AbstractSyntaxTree[⊤, ⊥, 𝒾(p), ¬p, p ∧ q, p ∨ q, p → q, p ↮ q, p ← q, p ↑ q, p ↓ q, p ↛ q, p ↔ q, p ↚ q]

#     conjunction(rs)
#     disjunction(rs)
#     print_table(rs)

#     for r in rs
#         collect(operators(r))
#         normalize(¬, r)
#         normalize(∨, r)
#         tseytin(r)
#         print_dimacs(r)
#         print_tree(r)
#         print(r)

#         for solution in solutions(r)
#             collect(solution)
#         end
#     end
# end end

end # PAndQ
