
import Base: mapfoldl, mapfoldr

"""
    arity(::LogicalOperator)

Returns the [arity](https://en.wikipedia.org/wiki/Arity)
of the given [`LogicalOperator`](@ref).

# Examples
```jldoctest
julia> arity(tautology)
0

julia> arity(not)
1

julia> arity(and)
2
```
"""
arity(::NullaryOperator) = 0
arity(::UnaryOperator) = 1
arity(::BinaryOperator) = 2

define_atom(p::Symbol) = :(const $p = $(Atom(p)))

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
    esc(quote
        $(map(define_atom, ps)...)
        Atom{Symbol}[$(ps...)]
    end)
end
#=
Source:
Symbolics.jl
https://github.com/JuliaSymbolics/Symbolics.jl
=#

atomize(p::Symbol) = :((@isdefined $p) ? $p : $(Atom(p)))
atomize(x::Expr) = Expr(x.head,
    (Meta.isexpr(x, (:(=), :kw)) ? identity : atomize)(x.args[1]),
    map(atomize, x.args[2:end])...
)
atomize(x) = x

"""
    @p(expression)

Instantiates each undefined variable
(ignoring variable assignment and keyword arguments)
as an [`Atom{Symbol}`](@ref).

# Examples
```jldoctest
julia> @p x = p
Atom:
 p

julia> @p x ∧ q → r
Tree:
 (p ∧ q) → r
```
"""
macro p(expression)
    esc(:($(atomize(expression))))
end

"""
    @p_str(x)

# Examples
```jldoctest
julia> p = @p_str("x")
Atom:
 x

julia> p"p ∧ q, Clause(and)"
(x ∧ q, ⊤)
```
"""
macro p_str(p)
    esc(:(@p $(Meta.parse(p))))
end

_atoms(p::Atom) = [p]
_atoms(p::Literal) = atoms(p.atom)
_atoms(p) = mapreduce(atoms, vcat, only_field(p); init = Atom[])

"""
    atoms(::Proposition)

Returns a vector of unique [`Atom`](@ref)s
contained in the given [`Proposition`](@ref).

!!! warning
    Some atoms may optimized out of an expression, such as in `p ∧ ⊥ == ⊥`.

# Examples
```jldoctest
julia> @p atoms(¬p)
1-element Vector{Atom{Symbol}}:
 p

julia> @p atoms(p ∧ q)
2-element Vector{Atom}:
 p
 q
```
"""
atoms(p::Proposition) = unique!(_atoms(p))
atoms(p::NullaryOperator) = Atom[]

# Reductions

"""
    ⋀(ps)
    conjunction(ps)

Equivalent to `foldl(and, ps; init = ⊤)`.

`⋀` can be typed by `\\bigwedge<tab>`.

See also [`and`](@ref).

# Examples
```jldoctest
julia> @p ⋀([p, q, r, s])
Tree:
 ((p ∧ q) ∧ r) ∧ s
```
"""
conjunction(ps) = foldl(and, ps)
const ⋀ = conjunction

"""
    ⋁(ps)
    disjunction(ps)

Equivalent to `foldl(or, ps; init = ⊥)`.

`⋁` can be typed by `\\bigvee<tab>`.

See also [`or`](@ref).

# Examples
```jldoctest
julia> @p ⋁([p, q, r, s])
Tree:
 ((p ∨ q) ∨ r) ∨ s
```
"""
disjunction(ps) = foldl(or, ps)
const ⋁ = disjunction

"""
    mapfoldl(f, lio::LeftNeutralOperator, ps)

Equivalent to `mapfoldl(f, lio, ps; init = only(left_neutrals(lio)))`

!!! tip
    This also works with `foldl(lio, ps)`.

# Examples
```jldoctest
julia> @p mapfoldl(not, and, [p, q, r, s])
Tree:
 ((¬p ∧ ¬q) ∧ ¬r) ∧ ¬s

julia> foldl(and, [])
tautology (generic function with 1 method)
```
"""
mapfoldl(f, lio::LeftNeutralOperator, ps) =
    mapfoldl(f, lio, ps, init = only(left_neutrals(lio)))

"""
    mapfoldr(f, rio::RightNeutralOperator, ps)

Equivalent to `mapfoldr(f, rio, ps; init = only(right_neutrals(rio)))`

!!! tip
    This also works with `foldr(rio, ps)`.

# Examples
```jldoctest
julia> @p mapfoldr(not, and, [p, q, r, s])
Tree:
 ¬p ∧ (¬q ∧ (¬r ∧ ¬s))

julia> foldr(and, [])
tautology (generic function with 1 method)
```
"""
mapfoldr(f, rio::RightNeutralOperator, ps) =
    mapfoldr(f, rio, ps, init = only(right_neutrals(rio)))
