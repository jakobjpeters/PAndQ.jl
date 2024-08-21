
module Interface

import Base: showerror
using Base: isexpr
using PAndQ

export
    Associativity, left, Operator, right,
    arity, dual, evaluate, initial_value, is_associative, is_commutative,
    is_root, name, parenthesize, print_expression, print_proposition, symbol

"""
    Operator{O}
    Operator{O}()

Return an operator named `O`.

Operators are uniquely identified by their name.
If possible, an operator should be defined as
`const o = ℴ = Operator{:o}()` where [`symbol`](@ref Interface.symbol)`(ℴ) == "ℴ"`.

This method is required to instantiate an operator.
"""
struct Operator{O} end

# Internals

"""
    InterfaceError{F <: Function, O <: Operator} <: Exception
    InterfaceError(::F, ::O)

An `Exception` indicating that the function of type `F` has not been implemented for the value of type `T`.
"""
struct InterfaceError{F, O <: Operator} <: Exception
    f::F
    o::O
end

"""
    showerror(::IO, ::InterfaceError)

Print a message indicating to implement a method of an interface.
"""
showerror(io::IO, e::InterfaceError) = print(io, "InterfaceError: implement `", e.f, "` for `", e.o, "`")

"""
    @interface(f, xs...)

Define a fallback method that throws an [`InterfaceError`](@ref Interface.InterfaceError).
"""
macro interface(f, xs...)
    esc(:(
        $(Expr(:call, f, map(x -> x == :o ? Expr(Symbol("::"), x, :Operator) : x, xs)...))
    = throw(InterfaceError($f, o))))
end

# Methods

## Evaluation

"""
    evaluate(::Symbol, ps)

Define the semantics of the [`Operator`](@ref Interface.Operator).

This method is required to [`normalize`](@ref) a proposition containing the given operator.

# Examples
```jldoctest
julia> @atomize Interface.evaluate(:not, [¬p])
p

julia> @atomize Interface.evaluate(:imply, [p, q])
¬p ∨ q
```
"""
function evaluate end

## Folding

"""
    Associativity

A trait to specify the associativity of an [`Operator`](@ref Interface.Operator).

This method is required for calling [`fold`](@ref) over the operator.

!!! note
    This trait is used internally and does not override how expressions are parsed.

Supertype of [`Left`]() and [`Right`]().
"""
@enum Associativity left right

"""
    initial_value(ℴ::Operator)

Specify a neutral value, `v`, of a binary [`Operator`](@ref Interface.Operator) such that `ℴ(v, p) == p`.

If there is no such neutral value, return `nothing`.

This method is required for calling [`fold`](@ref) over the operator.

See also [`==`](@ref).

# Examples
```jldoctest
julia> Interface.initial_value(∧)
⊤

julia> Interface.initial_value(∨)
⊥

julia> Interface.initial_value(↑)
```
"""
function initial_value end

## Printing

"""
    print_expression(io, ::Operator, ps)

Print the node of a syntax tree containing the [`Operator`](@ref Interface.Operator) and its propositions.

If a node in a syntax tree is not the root node, it may be necessary to parenthesize it to avoid ambiguity.
This context can be obtained using [`is_root`](@ref Interface.is_root).

Each proposition should be represented using [`print_proposition`](@ref).

This method is required for calling `show(::IO, ::MIME"text/plain, p)`
for a proposition `p` containing the given operator.

# Examples
```jldoctest
julia> @atomize Interface.print_expression(stdout, Interface.name(⊤), [])
⊤

julia> @atomize Interface.print_expression(stdout, Interface.name(¬), [p])
¬p

julia> @atomize Interface.print_expression(stdout, Interface.name(∧), [p, q])
p ∧ q
```
"""
@interface print_expression io o ps

"""
    symbol(ℴ::Operator)

Return the Unicode symbol of the [`Operator`](@ref).

If possible, this should be implemented as `symbol(::typeof(ℴ)) = "ℴ"`.

This method is required for calling `show(::IO, ::MIME"text/plain", ::typeof(ℴ))`.

See also [`show`](@ref).

# Examples
```jldoctest
julia> Interface.symbol(⊤)
"⊤"

julia> Interface.symbol(¬)
"¬"

julia> Interface.symbol(∧)
"∧"
```
"""
@interface symbol o

# Utilities

## Printing

"""
    name(::Operator{O})

Return `O`, the name of the [`Operator`](@ref Interface.Operator).

# Examples
```jldoctest
julia> Interface.name(⊤)
:tautology

julia> Interface.name(¬)
:not

julia> Interface.name(∧)
:and
```
"""
name(::Operator{O}) where O = O

"""
    is_root(io)

Return a `Bool`ean indicating whether the node being printed is the root of a syntax tree.
"""
is_root(io) = get(io, :root, true)

"""
    parenthesize(f, io)

Call `f`. If not [`is_root`](@ref Interface.is_root),
print opening and closing parentheses before and after, respectively.
"""
function parenthesize(f, io)
    root = is_root(io)
    root || print(io, "(")
    f()
    root || print(io, ")")
    nothing
end

"""
    print_proposition(io, p)

Print the given proposition with the `IOContext` that `:root => false`.

Should be called from [`print_expression`](@ref Interface.print_expression).

# Examples
```jldoctest
julia> @atomize print_proposition(stdout, ¬p)
¬p

julia> @atomize print_proposition(stdout, p ∧ q)
(p ∧ q)
```
"""
function print_proposition end

## Properties

"""
    arity(::Operator)

Return the number of propositions accepted by the [`Operator`](@ref Interface.Operator).

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
function arity end

"""
    dual(ℴ::Operator)

Return a function such that `dual(ℴ)(ps...) == ¬(ℴ(map(¬, ps)...))`.

If possible, this method should be implemented to return another [`Operator`](@ref Interface.Operator).

See also [`not`](@ref) and [`==`](@ref).

# Examples
```jldoctest
julia> Interface.dual(and)
∨

julia> Interface.dual(imply)
↚
```
"""
function dual end

### Predicates

"""
    is_associative(ℴ::Operator)

Return a `Bool`ean indicating whether the operator has the associative property
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
function is_associative end

"""
    is_commutative(ℴ::Operator)

Return a `Bool`ean indicating whether operator has the commutative property
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
function is_commutative end

end # Interface
