
# Extensions

## Markdown

```@docs
pretty_table(::Type{Markdown.MD}, ::TruthTable)
```

## Latexify

!!! tip
    A [`@latexrecipe`](https://korsbo.github.io/Latexify.jl/stable/tutorials/recipes/)
    has been defined for [`LogicalOperator`](@ref PAndQ.LogicalOperator), [`Proposition`](@ref PAndQ.Proposition), and [`TruthTable`](@ref).

```@docs
formatter(::Type{Latexify.LaTeXString})
pretty_table(::Type{Latexify.LaTeXString}, ::Union{PAndQ.Proposition, TruthTable})
```
