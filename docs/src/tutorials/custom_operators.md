
# [Custom Operators](@id custom_operators)

This tutorial will demonstrate how to implement custom operators using the operator [Interface](@ref interface). This interface can be used to implement operators with custom behavior such as:

- Number of parameters
- Lazy and eager evaluation
- Semantics
- Associativity
- Initial value
- Pretty printing
- Side-effects

## Setup

Implementing an operator requires defining methods for that operator. To do so, their function names must be imported or prefixed by the `Interface` module. This module also exports several other required and useful functions.

```@repl 1
import PAndQ:
    Associativity, Evaluation,
    arity, dual, evaluate, initial_value, pretty_print, symbol_of
using PAndQ
using .Interface
```

## Nullary

This is a renamed [`tautology`](@ref) operator. First, define an [`Operator`](@ref Interface.Operator). If possible, this should be a `const`ant whose name corresponds to the operator name.

```julia
julia> const truth = Operator{:truth}()
Error showing value of type Operator{:truth}:
ERROR: InterfaceError: implement `symbol_of` for `Operator{:truth}()`
```

If a required method is not implemented, a runtime error will display the function and operator that a method must be implemented for. The error says to implement [`symbol_of`](@ref Interface.symbol_of). This function is used to represent an operator.

```@setup 1
const truth = Operator{:truth}()
```

```@repl 1
symbol_of(::typeof(truth)) = "truth";
truth
truth()
```

The error says to implement [`Evaluation`](@ref Interface.Evaluation). This function is used to specify whether an operator lazily or eagerly evaluates its arguments.

```@repl 1
Evaluation(::typeof(truth)) = Lazy();
truth()
```

The error says to implement [`arity`](@ref Interface.arity). This function is used to construct a node in a syntax tree.

```@repl 1
arity(::typeof(truth)) = 0;
```

```julia
truth
```

The error says to implement [`pretty_print`](@ref Interface.pretty_print). This function is used to represent a node of a syntax tree. The [`show_proposition`](@ref Interface.show_proposition) function is used to represent the propositions in a node.

```@repl 1
pretty_print(io, o::typeof(truth)) = show(io, MIME"text/plain"(), o);
TruthTable([truth])
```

The error says to implement [`evaluate`](@ref Interface.evaluate). This function is used to specify the semantics of an operator.

```@repl 1
evaluate(::typeof(truth)) = ⊤;
TruthTable([truth])
```

## Unary

This is an eagerly evaluated [`not`](@ref) operator.

```@repl 1
const negate = Operator{:negate}();
symbol_of(::typeof(negate)) = "negate";
negate
Evaluation(::typeof(negate), p) = Eager();
evaluate(::typeof(negate), p) = evaluate(¬, p);
@atomize negate(¬p)
@atomize TruthTable([negate(p)])
```

## Binary

This is an [`imply`](@ref) operator represented by the `-->` symbol.

```@repl 1
const if_then = --> = Operator{:if_then}();
symbol_of(::typeof(-->)) = "-->";
-->
Evaluation(::typeof(-->), p, q) = Lazy();
arity(::typeof(-->)) = 2;
function pretty_print(io, o::typeof(-->), p, q)
    root = io[:root]
    root || print(io, "(")
    show_proposition(io, p)
    print(io, " ")
    show(io, MIME"text/plain"(), o)
    print(io, " ")
    show_proposition(io, q)
    root || print(io, ")")
end
@atomize p --> q
evaluate(::typeof(-->), p, q) = p → q;
@atomize TruthTable([p --> q])
@atomize fold(𝒾, (-->) => ())
```

This error says to implement [`Associativity`](@ref Interface.Associativity). This function is used to determine which direction to [`fold`](@ref).

```@repl 1
Associativity(::typeof(-->)) = Left();
@atomize fold(𝒾, (-->) => ())
```

This error says to implement [`initial_value`](@ref). This function is used to determine the `init` parameter when folding.

```@repl 1
initial_value(::typeof(-->)) = Some(⊤);
@atomize fold(𝒾, (-->) => ())
@atomize fold(𝒾, (-->) => (p, q, r))
```

## Ternary

This is a lazily evaluated conditional operator.

```@repl 1
const conditional = Operator{:conditional}();
symbol_of(::typeof(conditional)) = "?";
conditional
Evaluation(::typeof(conditional), p, q, r) = Lazy();
arity(::typeof(conditional)) = 3;
function pretty_print(io, o::typeof(conditional), p, q, r)
    root = io[:root]
    root || print(io, "(")
    show_proposition(io, p)
    print(io, " ? ")
    show_proposition(io, q)
    print(io, " : ")
    show_proposition(io, r)
    root || print(io, ")")
end;
@atomize ¬conditional(p, q, r)
evaluate(::typeof(conditional), p, q, r) = (p → q) ∧ (p ∨ r);
@atomize TruthTable([conditional(p, q, r)])
```

```julia
julia> @atomize ❓(truth, negate(p), ampersand(p, q))
(true ❓ !p : p & q)
```
