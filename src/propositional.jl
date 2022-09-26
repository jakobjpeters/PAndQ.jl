
import Base.⊻, Base.⊼, Base.⊽

abstract type Boolean <: Operator end
struct Not <: Boolean end
struct And <: Boolean end

struct Proposition{S <: Union{String, Nothing}}
    statement::S
end

Proposition() = Proposition(nothing)

struct P <: Language
    ϕ::Union{
        Proposition,
        Tuple{Not, Language},
        Tuple{And, Language, Language},
    }
end

abstract type Valuation end
struct ⊤ <: Valuation end
struct ⊥ <: Valuation end
(::⊤)() = P(Proposition())
(::⊥)() = ¬(⊤())
(::Proposition)() = ⊤
(::Not)(::Type{⊤}) = ⊥
(::Not)(::Type{⊥}) = ⊤
(::And)(p, q) = ⊥
(::And)(::Type{⊤}, ::Type{⊤}) = ⊤

# logical operators
¬(p::P) = P((Not(), p))               # \neg - not p
¬(p::Proposition) = ¬P(p)
¬(p) = ¬p()
∧(p::P, q::P) = P(((And(), p, q)))    # \wedge - p and q
∧(p::Proposition, q) = P(p) ∧ q
∧(p, q) = q() ∧ p
⊼(p, q) = ¬(p ∧ q)                    # \nand - not (p and q)
⊽(p, q) = ¬p ∧ ¬q                     # \nor - not (p or q)
∨(p, q) = ¬(p ⊽ q)                    # \vee - p or q
⊻(p, q) = (p ∨ q) ∧ (p ⊼ q)           # \veebar - (p or q) and not (p and q)
→(p, q) = ¬(p ∧ ¬q)                   # \rightarrow - if p then q
←(p, q) = q → p                       # \leftarrow - if q then p
↔(p, q) = (p → q) ∧ (p ← q)           # \leftrightarrow - if and only if p then q

function truth_table(operator)
    # @infix pairs = [⊤, ⊥] ⨉ [⊥, ⊤]
    pairs = ⨉([⊤, ⊥], [⊥, ⊤])
    return map(p_q -> p_q => operator(p_q...)(), pairs)
end

function truth_table(operator::typeof(¬))
    return map(p -> p => operator(p)(), [⊤, ⊥])
end

length(p::Proposition) = 1
length(ϕ::Tuple{Boolean, Vararg}) = 1 + mapreduce(length, +, Base.tail(ϕ))

print(p::T, indent) where T <: Proposition = print(repeat("  ", indent), p)

