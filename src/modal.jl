
abstract type Modal <: Operator end
struct Believe <: Modal end
struct Know <: Modal end

(operator::Modal)(agent, formula) = ϵ(formula, agent.operators[operator])

struct Agent{L <: Language}
    operations::Dict{<:Modal, Vector{L}} # Set{M} needs iterate(::Language) defined
end

length(ϕ::Tuple{Modal, Agent, Language}) = 1 + length(Base.tail(ϕ))
depth(ϕ::Tuple{Modal, Agent, Language}) = 1 + depth(Base.tail(ϕ))
depth(ϕ::Tuple{Boolean, Vararg}) = maximum(depth, Base.tail(ϕ))
depth(p::Proposition) = 0

struct ML <: Language
    ϕ::Union{
        PL,
        Tuple{Modal, Agent, ML}
    }
end

function (agent::Agent)(states)

end

function relate(agents, states)
    product = ⨉(states, states)
    return filter(agent -> agent(product), agents)
end

valuate(states::Vector) = map(valuate, states)

abstract type Model end
struct K <: Model end # no conditions
struct D <: Model end # serial
struct T <: Model end # reflexive
struct B <: Model end # reflexive and symmetric
struct S4 <: Model end # reflexive and transitive
struct S5 <: Model end # reflexive and Euclidean (includes symmetry and transitivity)
