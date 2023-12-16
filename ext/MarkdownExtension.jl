
module MarkdownExtension

import PAndQ: pretty_table
using Markdown: Markdown, MD
using PAndQ: NullaryOperator, TruthTable, Proposition, formatter

"""
    pretty_table(
        ::Type{Markdown.MD},
        ::Union{NullaryOperator, Proposition, TruthTable};
        formatters = formatter(NullaryOperator),
        alignment = :l
    )

See also [Nullary Operators](@ref nullary_operators),
[`Proposition`](@ref), [`TruthTable`](@ref), and [`formatter`](@ref).

# Examples
```jldoctest
julia> @atomize pretty_table(Markdown.MD, p ∧ q)
  p q p ∧ q
  – – –––––
  ⊤ ⊤ ⊤
  ⊥ ⊤ ⊥
  ⊤ ⊥ ⊥
  ⊥ ⊥ ⊥
```
"""
pretty_table(::Type{MD}, x::Union{NullaryOperator, Proposition, TruthTable}; backend = Val(:markdown), kwargs...) =
    Markdown.parse(pretty_table(String, x; backend, kwargs...))

end # module
