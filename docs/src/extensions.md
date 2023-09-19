
# Extensions

## Markdown

```@docs
pretty_table(::Type{Markdown.MD}, ::TruthTable)
```

## Latexify

!!! tip
    A [`@latexrecipe`](https://korsbo.github.io/Latexify.jl/stable/tutorials/recipes/)
    has been defined for
    [`PAndQ.LogicalOperator`](@ref), [`Proposition`](@ref), and [`TruthTable`](@ref).

```@docs
formatter(::Type{Latexify.LaTeXString})
pretty_table(::Type{Latexify.LaTeXString}, ::Union{Proposition, TruthTable})
```
