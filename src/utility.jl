
import Base: mapfoldl, mapfoldr, mapreduce

define_atom(p::Symbol) = :(const $(p) = $(Atom(p)))

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
macro atoms(ps...)
    return esc(:($(map(define_atom, ps)...); Atom{Symbol}[$(ps...)]))
end
#=
Source:
    https://github.com/JuliaSymbolics/Symbolics.jl
=#

atomize(p::String) = p |> Atom
atomize(p::Symbol) = :((@isdefined $p) ? $p : $(Atom(p)))
atomize(x::Expr) = Meta.isexpr(x, [:(=), :kw]) ?
    Expr(x.head, x.args[1], map(atomize, x.args[2:end])...) :
    Expr(x.head, map(atomize, x.args)...)
atomize(x) = x

"""
    @p(x)

Returns a propositions by instantiating all strings and undefined
variables as [`Atom`](@ref)s, and then evaluating the expression.

# Examples
```jldoctest
julia> x = @p p
Atom:
 p

julia> @p x ∧ q → "r"
Tree:
 (p ∧ q) → "r"
```
"""
macro p(x)
    return esc(:($(atomize(x))))
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
macro p_str(p)
    return esc(:(@p $(Meta.parse(p))))
end

__get_atoms(p::Union{Tree, Clause, Normal}) = mapreduce(get_atoms, vcat, getfield(p, 1))

# _get_atoms(::NullaryOperator) = Atom[]
_get_atoms(p::Atom) = [p]
_get_atoms(p::Literal) = get_atoms(p.atom)
_get_atoms(p::Union{Clause, Normal}) = isempty(getfield(p, 1)) ? Atom[] : __get_atoms(p)
_get_atoms(p::Valuation) =
    mapreduce(vcat, p.interpretations) do interpretation # Pair{Vector{Pair{Atom, Truth}}, Truth}
        map(first, first(interpretation))
    end
_get_atoms(p::Tree) = __get_atoms(p)
# _get_atoms(p::Proposition) = get_atoms(Tree(p)) # generic fallback

"""
    get_atoms(::Proposition)

Returns a vector of unique [`Atom`](@ref)s
contained in the given [`Proposition`](@ref).

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
mapfoldl(f, ::LIO, ps::AbstractArray) where LIO <: LeftIdentityOperator =
    mapfoldl(f, LIO.instance, ps, init = identity(:left, LIO.instance))

"""
    mapfoldr
"""
mapfoldr(f,::RIO, ps::AbstractArray) where RIO <: RightIdentityOperator =
    mapfoldr(f, RIO.instance, ps, init = identity(:right, RIO.instance))

"""
    mapreduce
"""
mapreduce(f, ::LIO, ps::AbstractArray) where LIO <: LeftIdentityOperator =
    mapfoldl(f, LIO.instance, ps)
mapreduce(f, ::BO, ps::AbstractArray) where BO <: Union{setdiff(
    Base.uniontypes(RightIdentityOperator),
    Base.uniontypes(LeftIdentityOperator)
)...} = mapfoldr(f, BO.instance, ps)

# import Base: rand
# rand(::Type{Proposition})
