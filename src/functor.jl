
(::Valuation{Val{:⊥}})(p = Primitive()) = p ∧ ¬p
(::Valuation{Val{:⊤}})(p = Primitive()) = ¬⊥()

(::Not)(p::Valuation{Val{:⊤}}) = ⊥
(::Not)(p::Valuation{Val{:⊥}}) = ⊤
(::Not)(p::Compound) = Propositional((_not, p))
(::Not)(p::Primitive) = ¬Propositional(p)
(::Not)(p::Propositional{Tuple{Not, Propositional{P}}}) where P <: Primitive = last(p.ϕ).ϕ # double negation elimination
(::Not)(p::Propositional{Tuple{Not, C}}) where C <: Compound = last(p.ϕ) # double negation elimination

(::And)(::Valuation{Val{:⊤}}, ::Valuation{Val{:⊤}}) = ⊤
(::And)(p, q) = q ∧ p # commutative law
(::And)(::Valuation{Val{:⊤}}, q::Language) = q # identity law
(::And)(::Valuation{Val{:⊥}}, ::Language) = ⊥ # domination law

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
