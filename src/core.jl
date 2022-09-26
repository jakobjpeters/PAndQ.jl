
abstract type Operator end
abstract type Language end

(ϕ::Tuple{Operator, Vararg})() = first(ϕ)(map(x -> x(), Base.tail(ϕ))...)
(ϕ::Language)() = ϕ.ϕ()

length(ϕ::Language) = length(ϕ.ϕ)
depth(ϕ::Language) = depth(ϕ.ϕ)

print(x::T, indent) where T <: Operator = print(repeat("  ", indent), T, "(), ")
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
