
import Base: mapfoldl, mapfoldr, mapreduce

define_atom(x::Symbol) = :(const $(x) = $(Atom(x)))

"""
    @atoms(ps...)

Instantiate and define [`Atom`](@ref)s with symbols and return a vector containing them.

!!! info
    Atoms are defined in the global scope as constants.

Examples
```jldoctest
julia> @atoms p q
2-element Vector{Atom{Symbol}}:
 p
 q

julia> p
Atom:
 p

julia> q
Atom:
 q
```
"""
macro atoms(p, qs...)
    rs = [p, qs...]
    return esc(quote
        $(map(define_atom, rs)...)
        [$(rs...)]
    end)
end
#=
Source:
    https://github.com/JuliaSymbolics/Symbolics.jl
=#

atomize(x::String) = Atom(x)
atomize(x::Symbol) = :((@isdefined $x) ? $x : $(Atom(x)))
function atomize(x::Expr)
    Meta.isexpr(x, [:(=), :kw]) && return Expr(x.head, x.args[1], map(atomize, x.args[2:end])...)
    return Expr(x.head, map(atomize, x.args)...)
end
atomize(x) = x

"""
    @p(x)

Returns a propositions by instantiating all strings and undefined
variables as [`Atom`](@ref)s, and then evaluating the expression.

# Examples
```jldoctest
julia> p = @p x
Atom:
 x

julia> @p p ∧ q → "r"
Tree:
 (x ∧ q) → "r"
```
"""
macro p(p)
    return esc(:($(atomize(p))))
end

"""
    @p_str(x)

# Examples
```jldoctest
julia> p = @p_str("x")
Atom:
 x

julia> p"\\"p\\" ∧ p, Clause(and)"
("p" ∧ x, ⊤)
```
"""
macro p_str(p) # TODO: `$` interpolation
    return esc(:(@p $(Meta.parse(p))))
end

__get_atoms(p::Union{Tree, Clause, Normal}) = mapreduce(get_atoms, vcat, getfield(p, 1))

# _get_atoms(::NullaryOperator) = Atom[]
_get_atoms(p::Atom) = [p]
_get_atoms(p::Literal) = get_atoms(p.atom)
_get_atoms(p::Union{Clause, Normal}) = isempty(getfield(p, 1)) ? Atom[] : __get_atoms(p)
_get_atoms(p::Valuation) = mapreduce(vcat, p.interpretations) do interpretation # Pair{Vector{Pair{Atom, Truth}}, Truth}
    map(first, first(interpretation))
end
_get_atoms(p::Tree) = __get_atoms(p)
# _get_atoms(p::Proposition) = get_atoms(Tree(p)) # generic fallback

"""
    get_atoms(::Proposition...)

Returns a vector of unique [`Atom`](@ref)s contained in the given [`Proposition`](@ref)(s).

!!! warning
    Some atoms may optimized out of an expression, such as in `p ∧ ⊥ == ⊥`.

# Examples
```jldoctest
julia> @p get_atoms(p ∧ q)
2-element Vector{Atom{Symbol}}:
 p
 q
```
"""
get_atoms(p::Proposition) = unique!(_get_atoms(p))

# Reductions

"""
    mapfoldl
"""
mapfoldl(f, ::LIO, xs::AbstractArray) where LIO <: LeftIdentityOperator =
    mapfoldl(f, LIO.instance, xs, init = identity(:left, LIO.instance))

"""
    mapfoldr
"""
mapfoldr(f,::RIO, xs::AbstractArray) where RIO <: RightIdentityOperator =
    mapfoldr(f, RIO.instance, xs, init = identity(:right, RIO.instance))

"""
    mapreduce
"""
mapreduce(f, ::LIO, xs::AbstractArray) where LIO <: LeftIdentityOperator = mapfoldl(f, LIO.instance, xs)
mapreduce(f, ::BO, xs::AbstractArray) where BO <: Union{
    setdiff(Base.uniontypes(RightIdentityOperator), Base.uniontypes(LeftIdentityOperator))...
} = mapfoldr(f, BO.instance, xs)

# import Base: rand
# rand(::Type{Proposition})
