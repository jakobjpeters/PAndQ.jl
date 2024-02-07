
```@meta
DocTestSetup = :(using PAndQ)
```

# Custom Operators

Implementing a custom operator can be useful to:

- Use a specific symbol
- Custom semantics
- Different number of parameters
- Eager vs lazy evaluation
- Folding associativity

```julia 1
julia> using PAndQ

julia> import PAndQ: Evaluation, FoldDirection, symbol_of, arity, show_proposition, evaluate, dual, initial_value

julia> using .Interface
```

## Nullary Operator

```julia
julia> const truth = Operator{:truth}();

julia> symbol_of(::typeof(truth)) = "1";

julia> arity(::typeof(truth)) = 0;

julia> show_proposition(io, ::typeof(truth)) = show(io, MIME"text/plain"(), truth);

julia> Evaluation(::typeof(truth)) = Lazy();

julia> evaluate(::typeof(truth)) = ⊤;

julia> dual(::typeof(truth)) = ⊥;

julia> truth
1

julia> truth()
⊤
```

## Unary Operator

```julia 1
julia> const negate = Operator{:negate}();

julia> symbol_of(::typeof(negate)) = "!";

julia> arity(::typeof(negate)) = 1;

julia> function pretty_print(io, o::typeof(negate), p)
           show(io, MIME"text/plain"(), o)
           show_proposition(io, p)
       end

julia> Evaluation(::typeof(negate), p) = Lazy();

julia> evaluate(::typeof(negate), p) = ¬p;

julia> dual(::typeof(negate)) = negate;

julia> negate
!

julia> @atomize negate(p)
!p
```

## Binary Operator

```julia 1
julia> const ampersand = Operator{:ampersand}();

julia> symbol_of(::typeof(ampersand)) = "&";

julia> arity(::typeof(ampersand)) = 2;

julia> function show_proposition(io, o::typeof(ampersand), p, q)
           root = io[:root]
           root && print(io, "(")
           show_proposition(io, p)
           print(io, " ")
           show(io, MIME"text/plain"(), o)
           print(io, " ")
           show_proposition(io, q)
           root && print(io, ")")
       end

julia> Evaluation(::typeof(ampersand), p, q) = Lazy();

julia> evaluate(::typeof(ampersand), p, q) = p ∧ q;

julia> dual(::typeof(ampersand)) = ∨;

julia> FoldDirection(::typeof(ampersand)) = Right();

julia> initial_value(::typeof(ampersand)) = Some(truth);
```

## Ternary Operator

```julia
julia> const conditional = Operator{:conditional}();

julia> symbol_of(::typeof(conditional)) = "?";

julia> arity(::typeof(conditional)) = 3;

julia> show_proposition(::typeof(conditional)) =
           (io, o, p, q, r) -> for x in ("(", p, " ", o, " ", q, " : ", r, ")")
                x isa String ? print(io, x) : show(io, MIME"text/plain"(), x)
           end

julia> Evaluation(::typeof(conditional), p, q, r) = Lazy();

julia> evaluate(::typeof(conditional), p, q, r) = (p → q) ∧ (p ∨ r);

julia> conditional
?

julia> @atomize conditional(p, q, r)
(p ? q : r)

julia> @atomize TruthTable([conditional(p, q, r)])
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
