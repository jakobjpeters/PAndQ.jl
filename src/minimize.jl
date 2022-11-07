
# double negation elimination
(::Not)(p::Propositional{Tuple{Not, Propositional{P}}}) where P <: Primitive = last(p.ϕ).ϕ
(::Not)(p::Propositional{Tuple{Not, Compound}}) = last(p.ϕ)

(::And)(p, q) = q ∧ p # commutative law
(::And)(p::Language, ::Valuation{Val{:⊤}}) = p # identity law
(::And)(::Language, q::Valuation{Val{:⊥}}) = q # domination law

# TODO: clean up
# type-unstable
# messy
function (::And)(p::Primitive, q::Language)
    # p() == q() && return p # idempotent law
    p == q && return p # idempotent law
    return Propositional(p) ∧ q
end
function (::And)(p::Compound, q::Compound)
    # p() == q() && return p # idempotent law
    p == q && return p # idempotent law
    return Propositional((_and, p, q))
end
