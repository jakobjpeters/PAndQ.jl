
# [Custom Operators](@id custom_operators)

This tutorial will demonstrate how to implement custom operators using the operator [Interface](@ref interface). This interface can be used to implement operators with custom behavior such as:

- Number of parameters
- Lazy and eager evaluation
- Semantics
- Associativity
- Initial value
- Pretty printing
- Side-effects

!!! warning
    This tutorial is not yet polished. This interface is incomplete and will be changed in v0.4.

## Setup

Implementing an operator requires defining methods for that operator. To do so, their function names must be imported or prefixed by the `Interface` module. This module also exports several other required and useful functions.

```@repl 1
import PAndQ:
    Associativity, Evaluation, dual, evaluate,
    initial_value, parenthesize, print_expression, symbol
using PAndQ, .Interface
```

## Nullary

This is a renamed [`tautology`](@ref) operator. First, define an [`Operator`](@ref Interface.Operator). If possible, this should be a `const`ant whose name corresponds to the operator name.

```
julia> const truth = Operator{:truth}()
Error showing value of type Operator{:truth}:
ERROR: InterfaceError: implement `symbol` for `Operator{:truth}()`
```

If a required method is not implemented, a runtime error will display the function and operator that a method must be implemented for. The error says to implement [`symbol`](@ref Interface.symbol). This function is used to print an operator.

```@setup 1
const truth = Operator{:truth}()
```

```@repl 1
symbol(::typeof(truth)) = "truth";
truth
truth()
```

The error says to implement [`Evaluation`](@ref Interface.Evaluation). This function is used to specify whether an operator lazily or eagerly evaluates its arguments.

```@setup 1
Evaluation(::typeof(truth)) = Lazy
```

```
julia> Evaluation(::typeof(truth)) = Lazy;

julia> truth()
Error showing value of type PAndQ.Tree{0}:
ERROR: InterfaceError: implement `print_expression` for `Operator{:truth}()` with `0` propositions
```

The error says to implement [`print_expression`](@ref Interface.print_expression). This function is used to print a node of a syntax tree.

```@repl 1
print_expression(io, o::typeof(truth), ps) = show(io, "text/plain", o);
truth()
print_table(truth())
```

The error says to implement [`evaluate`](@ref Interface.evaluate). This function is used to specify the semantics of an operator.

```@repl 1
evaluate(::typeof(truth), ps) = ⊤;
print_table(truth())
```

## Unary

This is an eagerly evaluated [`not`](@ref) operator.

```@repl 1
const negate = Operator{:negate}();
symbol(::typeof(negate)) = "negate";
negate
Evaluation(::typeof(negate)) = Eager;
evaluate(::typeof(negate), ps) = evaluate(¬, ps);
@atomize negate(¬p)
@atomize print_table(negate(p))
```

## Binary

This is an [`imply`](@ref) operator represented by the `-->` symbol.

```@repl 1
const if_then = --> = Operator{:if_then}();
symbol(::typeof(-->)) = "-->";
-->
Evaluation(::typeof(-->)) = Lazy;
```

If a node in a syntax tree is not the root node, it may be necessary to parenthesize it to avoid ambiguity. The [`parenthesize`](@ref Interface.parenthesize) function is used to print parentheses around a node if it is not the root node. The [`print_proposition`](@ref Interface.print_proposition) function is used to print the propositions in a node.

```@repl 1
print_expression(io, o::typeof(-->), ps) = parenthesize(io) do
    print_proposition(io, first(ps))
    print(io, " ")
    show(io, "text/plain", o)
    print(io, " ")
    print_proposition(io, last(ps))
end;
@atomize p --> q
evaluate(::typeof(-->), ps) = first(ps) → last(ps);
@atomize print_table(p --> q)
@atomize fold(𝒾, (-->) => ())
```

This error says to implement [`Associativity`](@ref Interface.Associativity). This function is used to determine which direction to [`fold`](@ref).

```@repl 1
Associativity(::typeof(-->)) = Left;
@atomize fold(𝒾, (-->) => ())
```

This error says to implement [`initial_value`](@ref Interface.initial_value). This function is used to determine the `init` parameter when [`fold`](@ref)ing.

```@repl 1
initial_value(::typeof(-->)) = ⊤;
@atomize fold(𝒾, (-->) => ())
@atomize fold(𝒾, (-->) => (p, q, r))
```

## Ternary

This is a lazily evaluated conditional operator.

```@repl 1
const conditional = Operator{:conditional}();
symbol(::typeof(conditional)) = "?";
conditional
Evaluation(::typeof(conditional)) = Lazy;
print_expression(io, o::typeof(conditional), ps) = parenthesize(io) do
    print_proposition(io, ps[1])
    print(io, " ? ")
    print_proposition(io, ps[2])
    print(io, " : ")
    print_proposition(io, ps[3])
end;
@atomize conditional(p, q, r)
function evaluate(::typeof(conditional), ps)
    p, q, r = ps
    (p → q) ∧ (p ∨ r)
end;
@atomize print_table(conditional(p, q, r))
```
