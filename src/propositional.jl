
import Base.⊼, Base.⊽, Base.⊻

struct Primitive{S <: Union{String, Nothing}}
    statement::S
end

Primitive() = Primitive(nothing)

#=
Source:
https://github.com/ctrekker/Deductive.jl
=#
macro primitive(expressions...)
    primitives = map(expression -> :($(esc(expression)) = Primitive($(string(expression)))), expressions)
    return quote
        $(primitives...)
        nothing
    end
end

(p::Primitive)(states) = get(states, p, Set([p]))

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
(::And)(p, q) = union(p, q)

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
¬(p::PL) = PL((Not(), p))              # not p
¬(p::Primitive) = ¬PL(p)
¬(p) = ¬p()
∧(p::PL, q::PL) = PL(((And(), p, q)))  # p and q
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

tautology = ⊤       # \top  
contradiction = ⊥   # \bot
not = ¬             # \neg
and = ∧             # \wedge
nand = ⊼            # \nand
nor = ⊽             # \nor
or = ∨              # \vee
xor = ⊻             # \veebar
imply_r = →         # \rightarrow
imply_l = ←         # \leftarrow
imply_r = ↔         # \leftrightarrow

length(p::Primitive) = 1
length(ϕ::Tuple{Boolean, Vararg}) = 1 + mapreduce(length, +, Base.tail(ϕ))

# show(io::IO, p::Primitive) = print("Primitive(\"", p.statement, "\")")
# print(p::Primitive, indent) = print(repeat("  ", indent), p)
