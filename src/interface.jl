
module Interface

import Base: showerror
using Base: isexpr
using PAndQ

export
    Associativity, Eager, Evaluation, Lazy, Left, Operator, Right,
    arity, converse, dual, evaluate, initial_value, is_associative,
    is_commutative, name_of, pretty_print, show_proposition, symbol_of

# Internals

"""
    InterfaceError{F, T} <: Exception
    InterfaceError(::F, ::T)
"""
struct InterfaceError{F, T} <: Exception
    f::F
    x::T

    InterfaceError(f::F, x::T) where {F, T} = new{F, T}(f, x)
end

"""
    showerror(::IO, ::InterfaceError)
"""
showerror(io::IO, e::InterfaceError) =
    print(io, "InterfaceError: implement `$(e.f)` for `$(e.x)`")

"""
    @interface(f, xs...)
"""
macro interface(f, xs...)
    esc(:(
        $(Expr(:call, f, map(x -> x == :o ? Expr(Symbol("::"), x, :Operator) : x, xs)...))
    = throw(InterfaceError($f, o))))
end

# Methods

"""
    Operator{O}
    Operator{O}()

Return an operator named `O`.

Operators are uniquely identified by their name.
If possible, an operator should be defined as
`const o = ℴ = Operator{:o}()` where [`symbol_of`](@ref Interface.symbol_of)`(ℴ) == "ℴ"`.

This method is required to instantiate an operator.
"""
struct Operator{O} end

"""
    arity(::Operator)

Return the number of propositions accepted by the [`Operator`](@ref Interface.Operator).

This method is required for [`Lazy`](@ref Interface.Lazy) operators.

# Examples
```jldoctest
julia> Interface.arity(⊤)
0

julia> Interface.arity(¬)
1

julia> Interface.arity(∧)
2
```
"""
@interface arity o

## Evaluation

"""
    Evaluation(::Operator, ps...)

A trait to specify the behavior of calling an [`Operator`](@ref Interface.Operator) with the given propositions.

This method is required to call the given operator.

Supertype of [`Eager`](@ref) and [`Lazy`](@ref).

# Examples
```jldoctest
julia> @atomize Interface.Evaluation(𝒾, p)
PAndQ.Interface.Eager()

julia> @atomize Interface.Evaluation(¬, p)
PAndQ.Interface.Lazy()
```
"""
abstract type Evaluation end
@interface Evaluation o ps...

"""
    evaluate(::Operator, ps...)

Define the semantics of the [`Operator`](@ref Interface.Operator).

This method is required to [`normalize`](@ref) a proposition containing the given operator.

# Examples
```jldoctest
julia> @atomize Interface.evaluate(¬, ¬p)
p

julia> @atomize Interface.evaluate(→, p, q)
¬p ∨ q
```
"""
@interface evaluate o ps...

## Folding

"""
    Associativity(ℴ::Operator)

A trait to specify the associativity of an [`Operator`](@ref Interface.Operator).

!!! note
    This trait is used internally and does not override how expressions are parsed.

This method is required for calling `fold` over `ℴ`.

Supertype of [`Left`](@ref) and [`Right`](@ref).

# Examples
```jldoctest
julia> Interface.Associativity(→)
PAndQ.Interface.Left()

julia> Interface.Associativity(←)
PAndQ.Interface.Right()
```
"""
abstract type Associativity end
@interface Associativity o

"""
    initial_value(ℴ::Operator)

Specify a neutral value, `v`, of a binary [`Operator`](@ref Interface.Operator) such that `ℴ(v, p) == p`.

To distinguish between an initial value and the absense of a neutral value,
return `Some(v)` or `nothing`, respectively.

This method is required for calling `fold` over `ℴ`.

See also [`==`](@ref).

# Examples
```jldoctest
julia> Interface.initial_value(∧)
Some(PAndQ.Interface.Operator{:tautology}())

julia> Interface.initial_value(∨)
Some(PAndQ.Interface.Operator{:contradiction}())

julia> Interface.initial_value(↑)
```
"""
@interface initial_value o

## Printing

"""
    pretty_print(io, ::Operator, ps...)

Represent the node of a syntax tree containing the [`Operator`](@ref Interface.Operator) and its propositions.

Nodes of a syntax tree may either be a root or a branch.
Some branches need to be parenthesized to avoid ambiguity.
This context can be obtained using `io[:root]`.

Each proposition should be represented using [`show_proposition`](@ref).

This method is required for calling `show(::IO, ::MIME"text/plain, p)`
for a proposition `p` containing the given operator.
"""
@interface pretty_print io o ps...

"""
    symbol_of(ℴ::Operator)

Return the Unicode symbol of the [`Operator`](@ref).

If possible, this should be implemented as `symbol_of(::typeof(ℴ)) = "ℴ"`.

This method is required for calling `show(::IO, ::MIME"text/plain", ::typeof(ℴ))`.

See also [`show`](@ref).

# Examples
```jldoctest
julia> Interface.symbol_of(⊤)
"⊤"

julia> Interface.symbol_of(¬)
"¬"

julia> Interface.symbol_of(∧)
"∧"
```
"""
@interface symbol_of o

# Utilities

## Evaluation

"""
    Eager <: Evaluation
    Eager()

A trait to specify that an [`Operator`](@ref Interface.Operator) is eagerly evaluated.

Eagerly evaluated operators return the expression specified by [`evaluate`](@ref Interface.evaluate).

Subtype of [`Evaluation`](@ref).
"""
struct Eager <: Evaluation end

"""
    Lazy <: Evaluation
    Lazy()

A trait to specify that an [`Operator`](@ref Interface.Operator) is lazily evaluated.

Lazily evaluated operators return a syntax tree.

Subtype of [`Evaluation`](@ref Interface.Evaluation).
"""
struct Lazy <: Evaluation end

## Folding

"""
    Left <: Associativity

A trait to specify that an [`Operator`](@ref Interface.Operator) is left-associative.

Subtype of [`Associativity`](@ref Interface.Associativity).
"""
struct Left <: Associativity end

"""
    Right <: Associativity

A trait to specify that an [`Operator`](@ref Interface.Operator) is right-associative.

Subtype of [`Associativity`](@ref Interface.Associativity).
"""
struct Right <: Associativity end

## Printing

"""
    name_of(::Operator{O})

Return `O`, the name of an [`Operator`](@ref Interface.Operator).
"""
name_of(::Operator{O}) where O = O

"""
    show_proposition(io, p)

Represent the given proposition with the `IOContext` that `:root => false`.

Should be called from [`pretty_print`](@ref Interface.pretty_print).

# Examples
```jldoctest
julia> @atomize show_proposition(stdout, ¬p)
¬p

julia> @atomize show_proposition(stdout, p ∧ q)
(p ∧ q)
```
"""
function show_proposition end

## Properties

"""
    converse(ℴ::Operator)

Return a function, `𝒸`, such that `converse(ℴ)(p, q) == 𝒸(q, p)`.

If possible, this method should be overloaded to return an [`Operator`](@ref Interface.Operator).

See also [`==`](@ref).

# Examples
```jldoctest
julia> Interface.converse(∧)
∧

julia> Interface.converse(→)
←
```
"""
converse(o::Operator) = (p, q) -> o(q, p)

"""
    dual(ℴ::Operator)

Return a function, `𝒹`, such that `¬ℴ(ps...) == 𝒹(map(¬, ps)...)`.

If possible, this method should be overloaded to return an [`Operator`](@ref Interface.Operator).

See also [`not`](@ref) and [`==`](@ref).

# Examples
```jldoctest
julia> Interface.dual(and)
∨

julia> Interface.dual(imply)
↚
```
"""
dual(o::Operator) = (ps...) -> map(¬, ¬normalize(∧, o(ps...)))

### Predicates

"""
    is_associative(ℴ::Operator)

Return a boolean indicating whether has the associative property
such that `ℴ(ℴ(p, q), r) == ℴ(p, ℴ(q, r))`.

See also [`==`](@ref).

# Examples
```jldoctest
julia> Interface.is_associative(∧)
true

julia> Interface.is_associative(→)
false
```
"""
function is_associative(o::Operator)
    p, q, r = map(Variable, (:p, :q, :r))
    ℴ(ℴ(p, q), r) == ℴ(p, ℴ(q, r))
end

"""
    is_commutative(ℴ::Operator)

Return a boolean indicating whether has the commutative property
such that `ℴ(p, q) == ℴ(q, p)`.

See also [`==`](@ref).

# Examples
```jldoctest
julia> Interface.is_commutative(∧)
true

julia> Interface.is_commutative(→)
false
```
"""
function is_commutative(o::Operator)
    p, q = map(Variable, (:p, :q))
    o(p, q) == o(q, p)
end

end