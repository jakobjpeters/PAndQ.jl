
```@meta
DocTestSetup = quote
    using PAQ
    @primitive p q r
end
```

# Tutorial

## Propositional Logic

### Primitive Propositions

A primitive proposition is a statement that can be true or false. For example, the statement "Logic is fun" may be true for you but false for someone else. [`Primitive`](@ref) propositions can be expressed as:

```jldoctest tutorial
julia> p = Primitive("Logic is fun")
Primitive("Logic is fun")

julia> q = Primitive("Julia is awesome")
Primitive("Julia is awesome")
```


### Compound Propositions

Since ```p``` can be true or false, we can form other logical statements that depends on ```p```'s truth value. These statements use logical connectives and are called [`Compound`](@ref) propositions. To express the proposition that "Logic is not fun", use the logical [`not`](@ref) connective: ```not(p)``` or ```¬p```.  If ```p```'s truth value is true, then ```¬p```'s truth value is false, and vice versa. A helpful tool to check a statement's truth values is [`@truth_table`](@ref).

```jldoctest tutorial
julia> @truth_table ¬p
┌───────────┬───────────────┐
│ p         │ ¬p            │
│ Primitive │ Propositional │
│ "p"       │               │
├───────────┼───────────────┤
│ ⊤         │ ⊥             │
│ ⊥         │ ⊤             │
└───────────┴───────────────┘
```

!!! info
    For now, think of the symbols ```⊤``` and ```⊥``` as *true* and *false*, respectively. An exact definition of them will be given in a couple of paragraphs.

Statements can also depend on multiple primitive propositions. The logical [`and`](@ref) connective is true when both ```p``` and ```q``` are true and is false otherwise. This is expressed as ```and(p, q)```, ```∧(p, q)```, or ```p ∧ q```. Repeatedly combining the connectives ```not``` and ```and``` can produce any possible truth table. As such, they are referred to as [functionally complete](https://en.wikipedia.org/wiki/Functional_completeness). For example, the connective [`or`](@ref) is equivalent to ```¬(¬p ∧ ¬q)```.

```jldoctest tutorial
julia> @truth_table or(p, q) ¬(¬p ∧ ¬q)
┌───────────┬───────────┬──────────────────────┐
│ p         │ q         │ or(p, q), ¬(¬p ∧ ¬q) │
│ Primitive │ Primitive │ Propositional        │
│ "p"       │ "q"       │                      │
├───────────┼───────────┼──────────────────────┤
│ ⊤         │ ⊤         │ ⊤                    │
│ ⊤         │ ⊥         │ ⊤                    │
├───────────┼───────────┼──────────────────────┤
│ ⊥         │ ⊤         │ ⊤                    │
│ ⊥         │ ⊥         │ ⊥                    │
└───────────┴───────────┴──────────────────────┘
```

!!! info
    The first two cells of each row in this table is an [interpretation](https://en.wikipedia.org/wiki/Interpretation_(logic)), which allows the truth value of the corresponding last cell to be determined. More generally, interpretations are an assignment of meaning to logical symbols. A function that maps logical symbols or formulae to their meaning is called a [valuation](https://en.wikipedia.org/wiki/Valuation_(logic)) function.


### Truth Values

Consider the proposition ```p ∧ ¬p```. Using the earlier example, this states that both "Logic is fun" and "Logic is not fun". Since these statements are mutually exclusive, their conjunction forms a [`contradiction`](@ref). A contradiction is a statement that is false in every possible interpretation. In other words, the statement ```p ∧ ¬p``` is false regardless of whether ```p```'s truth value is true or false. A contradiction can be expressed as ```contradiction``` or with the symbol ```⊥```. The negation of a contradiction, in this case ```¬(p ∧ ¬p)```, results in a statement that is true in every possible interpretation. This is called a [`tautology`](@ref) and can be expressed as ```tautology``` or with the symbol ```⊤```. Contradiction and tautology symbols are also be used to express the concepts of *true* and *false*, respectively.

!!! info
    Note that ```⊤``` is a Unicode symbol, not an uppercase "t". The documentation for each symbol provides instructions on how to type it. For example, ```⊤``` can be typed by ```\top<tab>```. See also Julia's documentation on [Unicode Input](https://docs.julialang.org/en/v1/manual/unicode-input/).

```jldoctest
julia> ¬⊥
⊤

julia> p ∧ ⊤ # identity law
Primitive("p")

julia> p ∧ ⊥ # domination law
⊥
```


## Implementation

### Types

```@example
using PAQ # hide
using InteractiveUtils # hide
using AbstractTrees # hide

AbstractTrees.children(x::Type) = InteractiveUtils.subtypes(x) # hide
print_tree(Language) # hide
```


In Backus-Naur Form (BNF), [`Propositional`](@ref) is defined inductively as:

```
ϕ ::= p | ¬ψ | ψ ∧ ψ
```

Since we may want to refer to compound statements defined differently, ψ has the abstract type [`Compound`](@ref) rather than being a ```Propositional```.



Remember, every infix operator is a function. They also each have a written alias.

```jldoctest tutorial
julia> p ∧ q === ∧(p, q) === and(p, q)
true
```


## Minimization

## Order of Operations

<!-- associativity -->