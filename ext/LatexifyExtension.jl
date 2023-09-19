
module LatexifyExtension

import Base: show
import Latexify: latexify
import PrettyTables: pretty_table
import PAndQ: __pretty_table, formatter
using Latexify: @latexrecipe, Latexify
using PrettyTables: LatexCell, LaTeXString
using PAndQ: Proposition, LogicalOperator, TruthTable, symbol_of, ___pretty_table, union_all_type, merge_string

@latexrecipe f(p::Proposition) = return Symbol(sprint(show, MIME"text/plain"(), p))
@latexrecipe f(lo::LogicalOperator) = return symbol_of(lo)

formatter(::Type{LaTeXString}) = (v, _, _) -> latexify(v)

__pretty_table(backend::Val{:latex}, io, truth_table; formatters = formatter(LaTeXString), kwargs...) =
    ___pretty_table(backend, io, truth_table.body; header = (
        map(cell -> LatexCell(join(map(latexify, cell), ", ")), truth_table.header),
        map(p -> merge_string(map(union_all_type, p)), truth_table.header)
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
\\begin{tabular}{|r|r|r|}
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

"""
    latexify(tt::TruthTable)

Equivalent to [`pretty_table(PrettyTables.LaTeXString, tt)`](@ref pretty_table).
"""
latexify(tt::TruthTable) = pretty_table(LaTeXString, tt)

"""
    show(::IO, ::MIME"text/latex", x::Union{Proposition, TruthTable})

Equivalent to [`print(io, latexify(x))`](@ref latexify).
"""
show(io::IO, ::MIME"text/latex", x::Union{Proposition, TruthTable}) = print(io, latexify(x))

end # module
