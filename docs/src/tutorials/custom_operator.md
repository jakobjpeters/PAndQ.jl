
```@meta
DocTestSetup = :(using PAndQ)
```

# Custom Operator

| Method                   | Description                                   |
|:-------------------------|:----------------------------------------------|
| [`Operator`](@ref)       | Instantiate the operator                      |
| [`show`](@ref)           | Pretty printing                               |
| [`arity`](@ref)          | Syntax [`Tree`](@ref PAndQ.Tree) construction |
| `evaluate`       | Semantics                                     |
| [`dual`](@ref)           |                                               |
| [`converse`](@ref)       |                                               |
| [`is_commutative`](@ref) |                                               |
| [`is_associative`](@ref) |                                               |

```julia 1
julia> import PAndQ: Evaluation, FoldDirection, symbol, arity, show_proposition, evaluate, dual, initial_value
```

## Motivation

- Represented by a specific symbol
- Different semantics
- Different number of arguments
- Eager vs lazy evaluation
- Folding

## Nullary Operator

```julia
julia> const truth = Operator{:truth}();

julia> symbol_of(::typeof(truth)) = "true";

julia> arity(::typeof(truth)) = 0;

julia> show_proposition(::typeof(truth)) = show_pretty;

julia> Evaluation(::typeof(truth)) = Lazy();

julia> evaluate(::typeof(truth)) = ⊤;

julia> dual(::typeof(truth)) = ⊥;
```

## Unary Operator

```julia 1
julia> const negate = ❗ = Operator{:negate}();

julia> symbol_of(::typeof(negate)) = "!";

julia> arity(::typeof(negate)) = 1;

julia> show_proposition(::typeof(negate)) = show_pretty;

julia> Evaluation(::typeof(negate), p) = Lazy();

julia> evaluate(::typeof(negate), p) = ¬p;

julia> dual(::typeof(negate)) = negate;
```

## Binary Operator

```julia 1
julia> const ampersand = Operator{:ampersand}();

julia> symbol_of(::typeof(ampersand)) = "&";

julia> arity(::typeof(ampersand)) = 2;

julia> show_proposition(::typeof(ampersand)) = show_pretty;

julia> Evaluation(::typeof(ampersand), p, q) = Lazy();

julia> evaluate(::typeof(ampersand), p, q) = p ∧ q;

julia> dual(::typeof(ampersand)) = ∨;

julia> FoldDirection(::typeof(ampersand)) = Right();

julia> initial_value(::typeof(ampersand)) = Some(truth);
```

## Ternary Operator

```julia
julia> const conditional = ❓ = Operator{:conditional}();

julia> symbol_of(::typeof(❓)) = "❓";

julia> arity(::typeof(❓)) = 3;

julia> show_proposition(::typeof(❓)) =
           (io, o, p, q, r) -> for x in ("(", p, " ", o, " ", q, " : ", r, ")")
                x isa String ? print(io, x) : show(io, MIME"text/plain"(), x)
           end

julia> Evaluation(::typeof(❓), p, q, r) = Lazy();

julia> evaluate(::typeof(❓), p, q, r) = (p → q) ∧ (p ∨ r);

julia> ❓
?

julia> @atomize ❓(p, q, r)
(p ? q : r)

julia> @atomize TruthTable([❓(p, q, r)])
┌───┬───┬───┬─────────────┐
│ p │ q │ r │ (p ? q : r) │
├───┼───┼───┼─────────────┤
│ ⊤ │ ⊤ │ ⊤ │ ⊤           │
│ ⊥ │ ⊤ │ ⊤ │ ⊤           │
├───┼───┼───┼─────────────┤
│ ⊤ │ ⊥ │ ⊤ │ ⊥           │
│ ⊥ │ ⊥ │ ⊤ │ ⊤           │
├───┼───┼───┼─────────────┤
│ ⊤ │ ⊤ │ ⊥ │ ⊤           │
│ ⊥ │ ⊤ │ ⊥ │ ⊥           │
├───┼───┼───┼─────────────┤
│ ⊤ │ ⊥ │ ⊥ │ ⊥           │
│ ⊥ │ ⊥ │ ⊥ │ ⊥           │
└───┴───┴───┴─────────────┘
```

```julia
julia> @atomize ❓(truth, negate(p), ampersand(p, q))
(true ❓ !p : p & q)
```
