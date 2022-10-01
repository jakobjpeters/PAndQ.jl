
import Base.⊼, Base.⊽, Base.⊻

struct Primitive{S <: Union{String, Nothing}}
    statement::S
end

Primitive() = Primitive(nothing)
(p::Primitive)(states) = get(states, p, Dict(p => [⊤, ⊥]))

abstract type Valuation end
struct ⊥ <: Valuation end # \bot
struct ⊤ <: Valuation end # \top
(::⊥)(p = Primitive()) = p ∧ ¬p
(::⊤)(p = Primitive()) = ¬⊥

abstract type Boolean <: Operator end
struct Not <: Boolean end
struct And <: Boolean end
(::Not)(::Type{⊥}) = ⊤
(::Not)(::Type{⊤}) = ⊥
(::Not)(p) = p
(::And)(::Type{⊤}, ::Type{⊤}) = ⊤
(::And)(p::Type, q::Type) = ⊥
(::And)(p, q) = merge(p, q)

struct PL <: Language
    ϕ::Union{
        Primitive,
        Tuple{Not, Language},
        Tuple{And, Language, Language},
    }
end

# Language(p, q::L) where L <: Language = L(p ∧ q)
# Language(p, qs::L...) where L <: Language = L(p ∧ first(qs), tail(qs))

# logical operators
¬(p::PL) = PL((Not(), p))               # not p
¬(p::Primitive) = ¬PL(p)
¬(p) = ¬p()
∧(p::PL, q::PL) = PL(((And(), p, q)))    # p and q
∧(p::Language, q) = q ∧ p
∧(p::Primitive, q) = PL(p) ∧ q
∧(p, q) = p() ∧ q
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

length(p::Primitive) = 1
length(ϕ::Tuple{Boolean, Vararg}) = 1 + mapreduce(length, +, Base.tail(ϕ))

print(p::T, indent) where T <: Primitive = print(repeat("  ", indent), p)
