
import Base: print, show

print(io::IO, p::Primitive, indent::Int) = print(io, repeat("  ", indent), p)
print(io::IO, operator::Operator, indent::Int) = print(io, repeat("  ", indent), operator, ", ")
print(io::IO, ϕ::Tuple{Operator, Vararg}, indent::Int) = map(arg -> print(io, arg, indent), ϕ)
print(io::IO, ϕ::Compound, indent::Int) = show(io, ϕ, indent)

function show(io::IO, ϕ::C, indent::Int = 0) where C <: Compound
    print(io, nameof(C), "(\n")
    print(io, ϕ.ϕ, indent + 1)
    print(io, "\n", repeat("  ", indent), ") ")
end
show(io::IO, ::Truth{V}) where V = print(io, first(V.parameters))
