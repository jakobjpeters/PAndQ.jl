
using Combinatorics

function (p::Language)()
    primitives = get_primitives(p)
    n = length(primitives)
    truth_sets = multiset_permutations([⊤, ⊥], [n, n], n)
    valuations = map(truth_set -> map(Pair{Primitive, Truth}, primitives, truth_set), truth_sets)
    truths = map(valuation -> interpret(p -> Dict(valuation)[p], p), valuations)
    return map(Pair, valuations, truths)
end

(::Not)(p::Truth{Val{:⊤}}) = ⊥
(::Not)(p::Truth{Val{:⊥}}) = ⊤
(::Not)(p::Language) = Propositional(_not, p)
(::Not)(p::Propositional{Tuple{Not, Propositional{P}}}) where P <: Primitive = last(p.ϕ).ϕ # double negation elimination
(::Not)(p::Propositional{Tuple{Not, C}}) where C <: Compound = last(p.ϕ) # double negation elimination

(::And)(::Truth{Val{:⊤}}, ::Truth{Val{:⊤}}) = ⊤
(::And)(::Truth{Val{:⊤}}, q::Language) = q # identity law
(::And)(::Truth{Val{:⊥}}, q::Language) = ⊥ # domination law
(::And)(p::Union{Primitive, Compound}, q::Truth) = q ∧ p # commutative law
(::And)(p::Language, q::Language) = Propositional(_and, p, q)
