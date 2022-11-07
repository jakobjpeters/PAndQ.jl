
show(io::IO, worlds::Vector{World}) = display(worlds)

print(x::O, indent) where O <: Operator = print(repeat("  ", indent), O, "(), ")
print(ϕ::Tuple{Operator, Vararg}, indent) = map(arg -> print(arg, indent), ϕ)
function Base.print(ϕ::C, indent = 0) where C <: Compound
    print(nameof(C), "(\n")
    print(ϕ.ϕ, indent + 1)
    print("\n", repeat("  ", indent), ") ")
end
show(io::IO, ϕ::Language) = print(ϕ)

print(p::Primitive, indent::Integer = 0) = print(repeat("  ", indent), p)
show(io::IO, p::Primitive{String}) = print("Primitive(\"", p.statement, "\")")
show(io::IO, p::Primitive{Nothing}) = print("Primitive(", p.statement, ")")
show(io::IO, ::Valuation{V, Primitive{Nothing}}) where V <: Val = print(first(V.parameters))
show(io::IO, v::Valuation{V, Primitive{String}}) where V <: Val = print(first(V.parameters), "(\"", v.p.statement, "\")")

function print_vector(io, xs)
    summary(io, xs)
    println(":")
    for x in xs[begin:end - 1]
        println(" ", x)
    end
    print(" ", last(xs))
end

function show(io::IO, ps::Vector{<:Primitive})
    print("Primitive[")
    for p in ps[begin:end - 1]
        if p isa Primitive{Nothing}
            print(p.statement, ", ")
        else
            print("\"", p.statement, "\", ")
        end
    end
    if last(ps) isa Primitive{Nothing}
        print(last(ps).statement, "]")
    else
        print("\"", last(ps).statement, "\"]")
    end
end
show(io::IOContext{Base.TTY}, ::MIME{Symbol("text/plain")}, ps::Vector{<:Primitive}) = print_vector(io, ps)
show(io::IOContext{Base.TTY}, ::MIME{Symbol("text/plain")}, vs::Vector{<:Valuation}) = print_vector(io, vs)
function show(io::IO, v::Valuation{V, VS}) where {V <: Val, VS <: Vector}
    print(first(V.parameters), "(")
    print(v.p)
    print(")")
end
