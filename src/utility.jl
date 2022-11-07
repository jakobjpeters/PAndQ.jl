
import Base.length, Base.print, Base.show

"""
    @primitive

Instantiate [`Primitive`](@ref) propositions.

# Examples
```jldoctest
julia> @primitive p q

julia> p

julia> q
```
"""
macro primitive(expressions...)
    primitive = expression -> :($(esc(expression)) = Primitive($(string(expression))))
    primitives = map(primitive, expressions)
    
    return quote
        $(primitives...)
        nothing
    end
end
#=
Source:
https://github.com/ctrekker/Deductive.jl
=#


depth(ϕ::Compound) = depth(ϕ.ϕ)
# depth(ϕ::Tuple{temp_modal, Agent, Language}) = 1 + depth(Base.tail(ϕ))
depth(ϕ::Tuple{Boolean, Vararg}) = maximum(depth, Base.tail(ϕ))
depth(p::Primitive) = 0

# length(ϕ::Compound) = length(ϕ.ϕ)
# length(p::Primitive) = 1
# length(ϕ::Tuple{Boolean, Vararg}) = 1 + mapreduce(length, +, Base.tail(ϕ))
# length(ϕ::Tuple{Modal, Agent, Language}) = 1 + length(Base.tail(ϕ))


# (p::Primitive)(states) = get(states, p, Set([p]))
# (::Not)(p) = p
# (::And)(p, q) = union(p, q)

struct World
    primitives::Tuple#{Primitive, DataType}
    valuation::DataType
end

# (ϕ::Tuple{Operator, Vararg})(states = Dict{Primitive, Union{Valuation, Vector{Valuation}}}()) = first(ϕ)(map(ϕ -> ϕ.ϕ(states), Base.tail(ϕ))...)
# function (ϕ::Language)()
#     primitives = ϕ.ϕ()
#     worlds = ⨉(map(primitive -> ⨉([primitive], [⊤, ⊥]), collect(primitives))...)
#     return vec(map(world -> World(world, ϕ.ϕ(Dict(world))), worlds))
# end
valuate(ϕ...) = reduce(∧, ϕ)()


# function infix(expression)
#     if expression isa Symbol
#         str_args = string(expression)
#         if length(str_args) == 1
#             return expression
#         end

#         indices = collect(eachindex(str_args)) # weird index behavior
#         first, rest = Symbol(str_args[indices[1]]), Symbol(str_args[indices[2:end]])
#         return Expr(:call, first, infix(rest))
#     end

#     return Expr(:call, infix(expression.args[1]), infix.(expression.args[2:end])...)
# end

# macro infix(expression)
#     return :($(expression.args[1]) = $(infix(expression.args[2].args[2])))
# end


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

# ≡(ϕ::Language, ψ) = ϕ() == ψ() # \equiv - logical equivalence
# equivalent = ≡
