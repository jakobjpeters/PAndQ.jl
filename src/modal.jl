
abstract type temp_modal <: Operator end
struct Believe <: temp_modal end
struct Know <: temp_modal end

struct Agent
    operations::Dict{<:temp_modal, Vector{Compound}} # Set{M} needs iterate(::Language) defined
end

struct Modal <: Compound
    ϕ::Tuple{temp_modal, Agent, Compound}
end










(operator::temp_modal)(agent, formula) = ϵ(formula, agent.operators[operator])

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
