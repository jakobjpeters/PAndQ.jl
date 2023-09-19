
module MarkdownExtension

import PAndQ: pretty_table, __pretty_table
using Markdown: Markdown, MD, Table
using PAndQ: NullaryOperator, TruthTable, Proposition, merge_string, symbol_of, formatter

__pretty_table(
    ::Val{:markdown}, io, truth_table;
    formatters = symbol_of, alignment = :l
) = print(io, MD(Table([
    map(merge_string, truth_table.header),
    eachrow(map(formatters, truth_table.body))...
], repeat([alignment], length(truth_table.header)))))

"""
    pretty_table(
        ::Type{Markdown.MD}, ::Union{Proposition, TruthTable};
        formatters = operator_symbol, alignment = :l
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
pretty_table(::Type{MD}, truth_table::TruthTable; kwargs...) =
    Markdown.parse(pretty_table(String, truth_table; kwargs...))
pretty_table(::Type{MD}, p::Proposition; kwargs...) =
    pretty_table(MD, TruthTable((p,)); kwargs...)

end # module
