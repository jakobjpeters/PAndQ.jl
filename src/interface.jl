
module Interface

import Base: showerror
using Base: isexpr
using PAndQ

export
    Associativity, Eager, Evaluation, Lazy, Left, Operator, Right,
    arity, converse, dual, evaluate, initial_value, is_associative,
    is_commutative, name, print_expression, print_proposition, symbol

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
    InterfaceError{F <: Function, O <: Operator, N <: Union{Nothing, Int}} <: Exception
    InterfaceError(::F, ::O, ::N)

An `Exception` indicating that the function of type `F` has not been implemented for the value of type `T`.
"""
struct InterfaceError{F, O <: Operator, N <: Union{Nothing, Int}} <: Exception
    f::F
    o::O
    n::N
end

"""
    showerror(::IO, ::InterfaceError)

Print a message indicating to implement a method of an interface.
"""
function showerror(io::IO, e::InterfaceError)
    n = e.n
    print(io, "InterfaceError: implement `", e.f, "` for `", e.o, "`")
    if !isnothing(n)
        print(io, " with `", n, "` propositions")
    end
end

"""
    @interface(f, xs...)

Define a fallback method that throws an [`InterfaceError`](@ref Interface.InterfaceError).
"""
macro interface(f, xs...)
    esc(:(
        $(Expr(:call, f, map(x -> x == :o ? Expr(Symbol("::"), x, :Operator) : x, xs)...))
    = throw(InterfaceError($f, o, $(:(ps...) in xs ? :(length(ps)) : nothing)))))
end

# Methods

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
    Evaluation(::Operator)

A trait to specify the behavior of calling an [`Operator`](@ref Interface.Operator).

This method is required to call the given operator.

Supertype of [`Eager`](@ref) and [`Lazy`](@ref).

# Examples
```jldoctest
julia> @atomize Interface.Evaluation(𝒾)
PAndQ.Interface.Eager

julia> @atomize Interface.Evaluation(¬)
PAndQ.Interface.Lazy
```
"""
abstract type Evaluation end
@interface Evaluation o

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
    Associativity(::Operator)

A trait to specify the associativity of an [`Operator`](@ref Interface.Operator).

This method is required for calling `fold` over the operator.

!!! note
    This trait is used internally and does not override how expressions are parsed.

Supertype of [`Left`](@ref) and [`Right`](@ref).

# Examples
```jldoctest
julia> Interface.Associativity(→)
PAndQ.Interface.Left

julia> Interface.Associativity(←)
PAndQ.Interface.Right
```
"""
abstract type Associativity end
@interface Associativity o

"""
    initial_value(ℴ::Operator)

Specify a neutral value, `v`, of a binary [`Operator`](@ref Interface.Operator) such that `ℴ(v, p) == p`.

To distinguish between an initial value and the absense thereof,
return `Some(v)` or `nothing`, respectively.

This method is required for calling `fold` over the operator.

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
    print_expression(io, ::Operator, ps...)

Print the node of a syntax tree containing the [`Operator`](@ref Interface.Operator) and its propositions.

Nodes of a syntax tree may either be a root or a branch.
Some branches need to be parenthesized to avoid ambiguity.
This context can be obtained using [`is_root`](@ref Interface.is_root).

Each proposition should be represented using [`print_proposition`](@ref).

This method is required for calling `show(::IO, ::MIME"text/plain, p)`
for a proposition `p` containing the given operator.

# Examples
```jldoctest
julia> @atomize Interface.print_expression(stdout, ⊤)
⊤

julia> @atomize Interface.print_expression(stdout, ¬, p)
¬p

julia> @atomize Interface.print_expression(stdout, ∧, p, q)
p ∧ q
```
"""
@interface print_expression io o ps...

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

## Evaluation

"""
    Eager <: Evaluation

A trait to specify that an [`Operator`](@ref Interface.Operator) is eagerly evaluated.

Eagerly evaluated operators return the expression specified by [`evaluate`](@ref Interface.evaluate).

Subtype of [`Evaluation`](@ref).
"""
struct Eager <: Evaluation end

"""
    Lazy <: Evaluation

A trait to specify that an [`Operator`](@ref Interface.Operator) is lazily evaluated.

Lazily evaluated operators return a syntax tree with the operator and its propositions as the root node.

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
    name(::Operator{O})

Return `O`, the name of an [`Operator`](@ref Interface.Operator).

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
    converse(ℴ::Operator)

Return a function such that `converse(ℴ)(p, q) == ℴ(q, p)`.

If possible, this method should be implemented to return another [`Operator`](@ref Interface.Operator).

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

Return a function such that `dual(ℴ)(ps...) == ¬(ℴ(map(¬, ps...)))`.

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
dual(o::Operator) = (ps...) -> map(¬, ¬normalize(∧, o(ps...)))

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
function is_associative(o::Operator)
    p, q, r = map(Variable, (:p, :q, :r))
    ℴ(ℴ(p, q), r) == ℴ(p, ℴ(q, r))
end

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
function is_commutative(o::Operator)
    p, q = map(Variable, (:p, :q))
    o(p, q) == o(q, p)
end

end