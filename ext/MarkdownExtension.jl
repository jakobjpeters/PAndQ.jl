
module MarkdownExtension

import Base: show
import PAndQ: pretty_table, __pretty_table
using Markdown: Markdown, MD, Table
using PAndQ: NullaryOperator, TruthTable, Proposition, symbol_of, formatter

__pretty_table(
    ::Val{:markdown}, io, tt;
    formatters = formatter(NullaryOperator), alignment
) = print(io, MD(Table(
    [tt.header, eachrow(map(v -> formatters(v, 0, 0), tt.body))...],
    repeat([alignment], length(tt.header))
)))

"""
    pretty_table(
        ::Type{Markdown.MD}, ::Union{Proposition, TruthTable};
        formatters = v -> v ? "⊤" : "⊥", alignment = :l
    )

# Examples
```jldoctest
julia> @atomize pretty_table(Markdown.MD, p ∧ q)
  p q p ∧ q
  – – –––––
  ⊤ ⊤ ⊤
  ⊥ ⊤ ⊥
  ⊤ ⊥ ⊥
  ⊥ ⊥ ⊥

julia> @atomize print(pretty_table(String, p ∧ q; backend = Val(:markdown)))
| p   | q   | p ∧ q |
|:--- |:--- |:----- |
| ⊤   | ⊤   | ⊤     |
| ⊥   | ⊤   | ⊥     |
| ⊤   | ⊥   | ⊥     |
| ⊥   | ⊥   | ⊥     |
```
"""
pretty_table(::Type{MD}, tt::TruthTable; backend = Val(:markdown), kwargs...) =
    Markdown.parse(pretty_table(String, tt; backend, kwargs...))
pretty_table(::Type{MD}, p::Proposition; kwargs...) =
    pretty_table(MD, TruthTable((p,)); kwargs...)

end # module
