
(::Valuation{Val{:⊥}})() = ⊥
(::Valuation{Val{:⊤}})() = ⊤

(::Not)(p::Valuation{Val{:⊤}}) = ⊥
(::Not)(p::Valuation{Val{:⊥}}) = ⊤
(::Not)(p::Union{Primitive, Compound}) = Propositional(_not, p)
(::Not)(p::Propositional{Tuple{Not, Propositional{P}}}) where P <: Primitive = last(p.ϕ).ϕ # double negation elimination
(::Not)(p::Propositional{Tuple{Not, C}}) where C <: Compound = last(p.ϕ) # double negation elimination

(::And)(::Valuation{Val{:⊤}}, ::Valuation{Val{:⊤}}) = ⊤
(::And)(p::Language, q::Language) = q ∧ p # commutative law
(::And)(::Valuation{Val{:⊤}}, q::Language) = q # identity law
(::And)(::Valuation{Val{:⊥}}, q::Language) = ⊥ # domination law

function (::And)(p::Union{Primitive, Compound}, q::Union{Primitive, Compound})
    # p() == q() && return p # idempotent law - type unstable
    p == q && return p # idempotent law - type unstable
    return Propositional(_and, p, q)
end
