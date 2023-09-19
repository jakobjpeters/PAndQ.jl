
module LatexifyExtension

import Base: show
import Latexify: latexify
import PrettyTables: pretty_table
import PAndQ: __pretty_table, formatter
using Latexify: @latexrecipe, Latexify
using PrettyTables: LatexCell, LaTeXString
using PAndQ: Proposition, LogicalOperator, TruthTable, symbol_of, ___pretty_table, union_all_type, merge_string

@latexrecipe f(lo::LogicalOperator) = return symbol_of(lo)
@latexrecipe f(p::Proposition) = return Symbol(sprint(show, MIME"text/plain"(), p))
@latexrecipe f(tt::TruthTable) = return pretty_table(LaTeXString, tt)

"""
    formatter(t::Type{Latexify.LaTeXString})

| `t`                    | `formatter(t)(⊤, _, _)` | `formatter(t)(⊥, _, _)` |
| :--------------------- | :---------------------- | :---------------------- |
| `Latexify.LaTeXString` | `"\$\\top\$"`           | `"\$\\bot\$"`           |
"""
formatter(::Type{LaTeXString}) = (v, _, _) -> string(latexify(v))

__pretty_table(backend::Val{:latex}, io, tt; formatters = formatter(LaTeXString), kwargs...) =
    ___pretty_table(backend, io, tt.body; header = (
        map(cell -> LatexCell(join(map(latexify, cell), ", ")), tt.header),
        map(p -> merge_string(map(union_all_type, p)), tt.header)
    ), formatters, kwargs...)

"""
    pretty_table(
        ::LaTexify.LaTexString, x::Union{Proposition, TruthTable};
        backend = Val(:latex), kwargs...
    )

Equivalent to [`LaTeXString(pretty_table(String, x; backend, kwargs...))`](@ref pretty_table).

# Examples
```jldoctest
julia> pretty_table(Latexify.LaTeXString, @atomize p ∧ q)
L"\\begin{tabular}{|r|r|r|}
  \\hline
  \\textbf{\$p\$} & \\textbf{\$q\$} & \\textbf{\$p \\wedge q\$} \\\\
  \\texttt{Variable} & \\texttt{Variable} & \\texttt{Tree} \\\\\\hline
  \$\\top\$ & \$\\top\$ & \$\\top\$ \\\\
  \$\\bot\$ & \$\\top\$ & \$\\bot\$ \\\\\\hline
  \$\\top\$ & \$\\bot\$ & \$\\bot\$ \\\\
  \$\\bot\$ & \$\\bot\$ & \$\\bot\$ \\\\\\hline
\\end{tabular}
"
```
"""
pretty_table(::Type{LaTeXString}, x::Union{Proposition, TruthTable}; backend = Val(:latex), kwargs...) =
    LaTeXString(pretty_table(String, x; backend, kwargs...))

end # module
