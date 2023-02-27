
import Base: foldl, mapfoldl, foldr, mapfoldr, reduce, mapreduce

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
    @p(p)

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

__get_atoms(p::Union{Tree, Clause, Normal}) = mapreduce(get_atoms, vcat, p.p)

# _get_atoms(::NullaryOperator) = Atom[]
_get_atoms(p::Atom) = [p]
_get_atoms(p::Literal) = get_atoms(p.p)
_get_atoms(p::Union{Clause, Normal}) = isempty(p.p) ? Atom[] : __get_atoms(p)
_get_atoms(p::Valuation) = mapreduce(vcat, p.p) do interpretation # Pair{Vector{Pair{Atom, Truth}}, Truth}
    map(first, first(interpretation))
end
_get_atoms(p::Tree) = __get_atoms(p)
# _get_atoms(p::Proposition) = get_atoms(Tree(p)) # generic fallback

"""
    get_atoms(::Proposition...)

Returns a vector of unique [`Atom`](@ref)s contained in the given [`Proposition`](@ref)(s).

!!! warning
    Some atoms may optimized out of an expression, such as in ```p ∧ ⊥ == ⊥```.

# Examples
```jldoctest
julia> @p get_atoms(p ∧ q)
2-element Vector{Atom{Symbol}}:
 p
 q
```
"""
get_atoms(p::Proposition) = unique!(_get_atoms(p))

"""
    foldl(binary_operator, ps)

Equivalent to `foldl(binary_operator, ps, init = identity(left, binary_operator))`.

See also [`identity`](@ref).

# Examples
```jldoctest
julia> foldl(and, [])
tautology (generic function with 1 method)

julia> @p foldl(and, [p, q, r, s])
Tree:
 ((p ∧ q) ∧ r) ∧ s

julia> @p foldl(imply, [p, q, r, s])
Tree:
 ((p → q) → r) → s
```
"""
foldl(
    bo::Union{
        typeof(and),
        typeof(or),
        typeof(xor),
        typeof(xnor),
        typeof(imply),
        typeof(not_converse_imply)
    },
    ps::AbstractArray
) = foldl(bo, ps, init = identity(left, bo))

"""
    mapfoldl
"""
mapfoldl(
    f,
    bo::Union{
        typeof(and),
        typeof(or),
        typeof(xor),
        typeof(xnor),
        typeof(imply),
        typeof(not_converse_imply)
    },
    ps::AbstractArray
) = mapfoldl(f, bo, ps, init = identity(left, bo))

"""
    foldr(binary_operator, ps)

Equivalent to `foldr(binary_operator, ps, init = identity(right, binary_operator))`.

See also [`identity`](@ref).

# Examples
```jldoctest
julia> foldr(and, [])
tautology (generic function with 1 method)

julia> @p foldr(and, [p, q, r, s])
Tree:
 p ∧ (q ∧ (r ∧ s))

julia> @p foldr(converse_imply, [p, q, r, s])
Tree:
 p ← (q ← (r ← s))
```
"""
foldr(
    bo::Union{
        typeof(and),
        typeof(or),
        typeof(xor),
        typeof(xnor),
        typeof(not_imply),
        typeof(converse_imply)
    },
    ps::AbstractArray
) = foldr(bo, ps, init = identity(right, bo))

"""
    mapfoldr
"""
mapfoldr(
    f,
    bo::Union{
        typeof(and),
        typeof(or),
        typeof(xor),
        typeof(xnor),
        typeof(not_imply),
        typeof(converse_imply)
    },
    ps::AbstractArray
) = mapfoldr(f, bo, ps, init = identity(right, bo))

"""
    reduce(binary_operator, ps)

Equivalent to `reduce(binary_operator, ps, init = identity(direction, binary_operator))`,
where `direction` is either `left` or `right`,
depending on which is valid for the given binary operator.

See also [`identity`](@ref).

# Examples
```jldoctest
julia> reduce(and, [])
tautology (generic function with 1 method)

julia> @p reduce(imply, [p, q, r, s])
Tree:
 ((p → q) → r) → s

julia> @p reduce(converse_imply, [p, q, r, s])
Tree:
 p ← (q ← (r ← s))
```
"""
reduce(
    bo::Union{
        typeof(and),
        typeof(or),
        typeof(xor),
        typeof(xnor),
        typeof(imply),
        typeof(not_converse_imply)
    },
    ps::AbstractArray
) = foldl(bo, ps)
reduce(bo::Union{typeof(not_imply), typeof(converse_imply)}, ps::AbstractArray) = foldr(bo, ps)

"""
    mapreduce
"""
mapreduce(
    f,
    bo::Union{
        typeof(and),
        typeof(or),
        typeof(xor),
        typeof(xnor),
        typeof(imply),
        typeof(not_converse_imply)
    },
    ps::AbstractArray
) = mapfoldl(f, bo, ps)
mapreduce(f, bo::Union{typeof(not_imply), typeof(converse_imply)}, ps::AbstractArray) = mapfoldr(f, bo, ps)

# import Base: rand
# rand(::Type{Proposition})
