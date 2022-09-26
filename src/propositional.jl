
import Base.⊼, Base.⊽, Base.⊻

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
¬(p::P) = P((Not(), p))               # not p
¬(p::Proposition) = ¬P(p)
¬(p) = ¬p()
∧(p::P, q::P) = P(((And(), p, q)))    # p and q
∧(p::Proposition, q) = P(p) ∧ q
∧(p, q) = q() ∧ p
⊼(p, q) = ¬(p ∧ q)                    # not (p and q)
⊽(p, q) = ¬p ∧ ¬q                     # not (p or q)
∨(p, q) = ¬(p ⊽ q)                    # p or q
⊻(p, q) = (p ∨ q) ∧ (p ⊼ q)           # (p or q) and not (p and q)
→(p, q) = ¬(p ∧ ¬q)                   # if p then q
←(p, q) = q → p                       # if q then p
↔(p, q) = (p → q) ∧ (p ← q)           # if p then q and if q then p

tautology = ⊤
contradiction = ⊥
not = ¬        # \neg
and = ∧        # \wedge
nand = ⊼       # \nand
nor = ⊽        # \nor
or = ∨         # \vee
xor = ⊻        # \veebar
imply_r = →    # \rightarrow
imply_l = ←    # \leftarrow
imply_r = ↔    # \leftrightarrow

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

