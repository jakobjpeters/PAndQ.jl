
module PAQ

include("set.jl")
include("types.jl")
include("functor.jl")
include("boolean_operators.jl")
# include("first_order.jl")
include("utility.jl")
include("pretty.jl")

export
    # utility.jl
    @primitive, @truth_table, get_primitives, depth, length, interpret, ==,

    # set.jl
    ⨉, cartesian_product,

    # types.jl
    Language, Compound, Primitive, Propositional, # propositions
    Truth, ⊤, ⊥, tautology, contradiction, # valuations

    # propositional logic
    Operator, Boolean, Not, And, # boolean operators
    ¬, ∧, ⊼, ⊽, ∨, ⊻, →, ↛, ←, ↚, ↔, # boolean operators
    not, and, nand, nor, or, xor, if_then, not_if_then, then_if, not_then_if, xnor # boolean operator alias'

    # first_order
    # ∀, ∃, ∄
return nothing
end # module
