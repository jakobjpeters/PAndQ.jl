
(::Truth{Val{:⊥}})() = ⊥
(::Truth{Val{:⊤}})() = ⊤

(::Not)(p::Truth{Val{:⊤}}) = ⊥
(::Not)(p::Truth{Val{:⊥}}) = ⊤
(::Not)(p::Language) = Propositional(_not, p)
(::Not)(p::Propositional{Tuple{Not, Propositional{P}}}) where P <: Primitive = last(p.ϕ).ϕ # double negation elimination
(::Not)(p::Propositional{Tuple{Not, C}}) where C <: Compound = last(p.ϕ) # double negation elimination

(::And)(::Truth{Val{:⊤}}, ::Truth{Val{:⊤}}) = ⊤
(::And)(::Truth{Val{:⊤}}, q::Language) = q # identity law
(::And)(::Truth{Val{:⊥}}, q::Language) = ⊥ # domination law
(::And)(p::Union{Primitive, Compound}, q::Truth) = q ∧ p # commutative law

function (::And)(p::Language, q::Language)
    # p() == q() && return p # idempotent law - type unstable
    p == q && return p # idempotent law - type unstable
    return Propositional(_and, p, q)
end
