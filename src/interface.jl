
module Interface

import Base: showerror
using Base: isexpr

export
    Operator, arity,
    Evaluation, Eager, Lazy, evaluate,
    FoldDirection, Left, Right, initial_value,
    symbol_of, pretty_print,
    is_associative, is_commutative,
    dual, converse

# Internals

"""
    InterfaceError{F, T} <: Exception
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
    @interface(xs...)
"""
macro interface(xs...)
    esc(:(
        $(Expr(:call, map(x -> x == :o ? Expr(Symbol("::"), x, :Operator) : x, xs)...))
    = throw(InterfaceError($(first(xs)), o))))
end

"""
    Operator{S}
    Operator{S}()

Instantiate a logical operator named `S`.

Operators are uniquely identified by their name.
"""
struct Operator{S} end

"""
    Evaluation(::Operator, ps...)

A trait to specify the behavior of calling an [`Operator`](@ref Interface.Operator) with the given parameters.

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
    Eager <: Evaluation
    Eager()

A trait to specify that an [`Operator`](@ref Interface.Operator) is eagerly evaluated.

Eagerly evaluated operators return the expression specified by [`evaluate`](@ref).

Subtype of [`Evaluation`](@ref).
"""
struct Eager <: Evaluation end

"""
    Lazy <: Evaluation
    Lazy()

A trait to specify that an [`Operator`](@ref Interface.Operator) is lazily evaluated.

Lazily evaluated operators return a syntax [`Tree`](@ref PAndQ.Tree).

Subtype of [`Evaluation`](@ref).
"""
struct Lazy <: Evaluation end

"""
    evaluate(::Operator, ps...)

Defines the semantics of the given [`Operator`](@ref Interface.Operator).

# Examples
```jldoctest
julia> @atomize Interface.evaluate(¬, ¬p)
p

julia> @atomize Interface.evaluate(→, p, q)
¬p ∨ q
```
"""
@interface evaluate o ps...

"""
    arity(::Operator)

Return the number of parameters accepted by the given [`Operator`](@ref Interface.Operator).

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

"""
    symbol_of(::Operator)

Return the Unicode symbol of the given [`Operator`](@ref).

This symbol is used to represent the operator when calling `show(::IO, ::MIME"text/plain", ::Operator)`.

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

"""
    pretty_print(io, ::Operator, ps...)

Represent the syntax tree of the given [`Operator`](@ref Interface.Operator) and its parameters.

Nodes of a syntax tree may either be a root or a branch.
Some branches need to be parenthesized to avoid ambiguity.
This context can be obtained using `io[:root]`.

Each parameter should be represented using [`show_proposition`](@ref).
"""
@interface pretty_print io o ps...

## Fold

"""
    initial_value(::Operator)

Specify a neutral value to be used in [`fold`](@ref) when there are no elements to fold over.

An [`Operator`](@ref Interface.Operator) without an intial value should return `nothing`,
wheras one with an intial value should wrap it in `Some`.

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

"""
    FoldDirection(::Operator)

A trait to indicate the associativity of an [`Operator`](@ref Interface.Operator) used in [`fold`](@ref).

Supertype of [`Left`](@ref) and [`Right`](@ref).

# Examples
```jldoctest
julia> Interface.FoldDirection(→)
PAndQ.Interface.Left()

julia> Interface.FoldDirection(←)
PAndQ.Interface.Right()
```
"""
abstract type FoldDirection end
@interface FoldDirection o

"""
    Left <: FoldDirection

A trait to indicate that an [`Operator`](@ref Interface.Operator) is left-associative when used in [`fold`](@ref).

Subtype of [`FoldDirection`](@ref).
"""
struct Left <: FoldDirection end

"""
    Right <: FoldDirection

A trait to indicate that an [`Operator`](@ref Interface.Operator) is right-associative when used in [`fold`](@ref).

Subtype of [`FoldDirection`](@ref).
"""
struct Right <: FoldDirection end

## Properties

"""
    dual(::Operator)

Returns the [`Operator`](@ref Interface.Operator) that is the [dual]
(https://en.wikipedia.org/wiki/Boolean_algebra#Duality_principle)
of the given operator.

# Examples
```jldoctest
julia> Interface.dual(and)
∨

julia> Interface.dual(imply)
↚
```
"""
dual(o::Operator) = (ps...) -> map(¬, ¬normalize(∧, o(ps...)))

"""
    converse(::Operator)

Returns the [`Operator`](@ref Interface.Operator) that is the
[converse](https://en.wikipedia.org/wiki/Converse_(logic))
of the given operator.

# Examples
```jldoctest
julia> Interface.converse(∧)
∧

julia> Interface.converse(→)
←
```
"""
@interface converse o

## Predicates

"""
    is_associative(::Operator)

Return a boolean indicating whether the given [`Operator`](@ref Interface.Operator) has the
[associative property](https://en.wikipedia.org/wiki/Associative_property).

# Examples
```jldoctest
julia> Interface.is_associative(∧)
true

julia> Interface.is_associative(→)
false
```
"""
@interface is_associative o

"""
    is_commutative(::Operator)

Return a boolean indicating whether the given [`Operator`](@ref Interface.Operator) has the
[commutative property](https://en.wikipedia.org/wiki/Commutative_property).

# Examples
```jldoctest
julia> Interface.is_commutative(∧)
true

julia> Interface.is_commutative(→)
false
```
"""
@interface is_commutative o

end