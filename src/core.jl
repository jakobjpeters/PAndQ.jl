
abstract type Operator end
abstract type Language end

struct World
    primitives
    valuation::DataType
end
# show(io::IO, world::World) = println(world.primitives, ", ", world.valuation)
# show(io::IO, worlds::Vector{World}) = print(eltype(worlds), "[\n", worlds..., "]")

(ϕ::Tuple{Operator, Vararg})(states = Dict{Primitive, Union{Valuation, Vector{Valuation}}}()) = first(ϕ)(map(ϕ -> ϕ.ϕ(states), Base.tail(ϕ))...)
function (ϕ::Language)()
    primitives = ϕ.ϕ()
    worlds = ⨉(map(primitive -> ⨉([primitive], [⊤, ⊥]), collect(primitives))...)
    return vec(map(world -> World(world, ϕ.ϕ(Dict(world))), worlds))
end

# function valuate(ϕ::Language...)

# end

length(ϕ::Language) = length(ϕ.ϕ)
depth(ϕ::Language) = depth(ϕ.ϕ)

print(x::O, indent) where O <: Operator = print(repeat("  ", indent), O, "(), ")
print(ϕ::Tuple{Operator, Vararg}, indent) = map(arg -> print(arg, indent), ϕ)
function print(ϕ::Language, indent = 0)
    print("Language(\n")
# function Base.print(ϕ::T, indent = 0) where T <: Language
#     print(T, "(\n")
    print(ϕ.ϕ, indent + 1)
    print("\n", repeat("  ", indent), ") ")
end
show(io::IO, ϕ::Language) = print(ϕ)

function infix(expression)
    if expression isa Symbol
        str_args = string(expression)
        if length(str_args) == 1
            return expression
        end

        indices = collect(eachindex(str_args)) # weird index behavior
        first, rest = Symbol(str_args[indices[1]]), Symbol(str_args[indices[2:end]])
        return Expr(:call, first, infix(rest))
    end

    return Expr(:call, infix(expression.args[1]), infix.(expression.args[2:end])...)
end

macro infix(expression)
    return :($(expression.args[1]) = $(infix(expression.args[2].args[2])))
end


# struct World
#     states::Dict{Language, Language}
# end

# #=
# returns
#     ⊤ if tautology
#     ⊥ if contradiction
#     ? if contingency
# =#
# # satisfiable
# # unsatisfiable

# # return vector of possible worlds
# # if no possible worlds, contradiction
# # if ϕ is in all possible worlds, tautology
# # if ϕ is in no possible worlds, contradiction
# prove(w::world, f::Language) = w.states[f](f)

# ⊢ # \vdash
# ⊣ # \dashv

# ≡(f1, f2) = truth_table(f1) == truth_table(f2) # \equiv - logical equivalence
# equivalent = ≡
