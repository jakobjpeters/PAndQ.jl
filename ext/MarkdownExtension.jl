
module MarkdownExtension

import PAndQ: show, pretty_table, _pretty_table
using Markdown: Markdown, MD, Table
using PAndQ: NullaryOperator, TruthTable, Proposition, symbol_of, formatter

_pretty_table(::Val{:markdown}, io, tt; formatters =
    formatter(NullaryOperator), alignment) = print(io, MD(Table(
        [tt.header, eachrow(map(v -> formatters(v, 0, 0), tt.body))...],
    repeat([alignment], length(tt.header)))))

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

julia> @atomize print(pretty_table(String, p ∧ q; backend = Val(:markdown)))
| p   | q   | p ∧ q |
|:--- |:--- |:----- |
| ⊤   | ⊤   | ⊤     |
| ⊥   | ⊤   | ⊥     |
| ⊤   | ⊥   | ⊥     |
| ⊥   | ⊥   | ⊥     |
```
"""
pretty_table(::Type{MD}, x::Union{NullaryOperator, Proposition, TruthTable}; backend = Val(:markdown), kwargs...) =
    Markdown.parse(pretty_table(String, x; backend, kwargs...))

end # module
