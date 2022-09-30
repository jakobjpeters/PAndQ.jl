
import Base.⊼, Base.⊽, Base.⊻

abstract type Boolean <: Operator end
struct Not <: Boolean end
struct And <: Boolean end

struct Proposition{S <: Union{String, Nothing}}
    statement::S
end

Proposition() = Proposition(nothing)

struct PL <: Language
    ϕ::Union{
        Proposition,
        Tuple{Not, Language},
        Tuple{And, Language, Language},
    }
end

Language(p, q::L) where L <: Language = L(p ∧ q)
Language(p, qs::L...) where L <: Language = L(p ∧ first(qs), tail(qs))

abstract type Valuation end
struct ⊤ <: Valuation end    # \top
struct ⊥ <: Valuation end    # \bot
(::⊤)() = PL(Proposition())
(::⊥)() = ¬(⊤())
(::Proposition)() = ⊤
(::Not)(::Type{⊤}) = ⊥
(::Not)(::Type{⊥}) = ⊤
(::And)(p, q) = ⊥
(::And)(::Type{⊤}, ::Type{⊤}) = ⊤

# logical operators
¬(p::PL) = PL((Not(), p))               # not p
¬(p::Proposition) = ¬PL(p)
¬(p) = ¬p()
∧(p::PL, q::PL) = PL(((And(), p, q)))    # p and q
∧(p::Proposition, q) = PL(p) ∧ q
∧(p, q) = q() ∧ p
⊼(p, q) = ¬(p ∧ q)                    # not (p and q)
⊽(p, q) = ¬p ∧ ¬q                     # not (p or q)
∨(p, q) = ¬(p ⊽ q)                    # p or q
⊻(p, q) = (p ∨ q) ∧ (p ⊼ q)           # (p or q) and not (p and q)
→(p, q) = ¬(p ∧ ¬q)                   # if p then q
←(p, q) = q → p                       # if q then p
↔(p, q) = (p → q) ∧ (p ← q)           # if p then q and if q then p

# make this a macro
# operator(params...) = (params...) -> operator(params)
# ¬(p) = p -> ¬p
# ∧(p, q) = (p, q) -> p ∧ q

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
