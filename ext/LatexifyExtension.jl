
module LatexifyExtension

import Base: show
import Latexify: latexify
import PAndQ: pretty_table, __pretty_table, formatter
using Latexify: @latexrecipe, Latexify
using PrettyTables: LatexCell, LaTeXString
using PAndQ: Proposition, Operator, TruthTable, symbol_of, ___pretty_table

@latexrecipe f(o::Operator) = return symbol_of(o)
@latexrecipe f(p::Proposition) = return Symbol(repr(MIME"text/plain"(), p))
@latexrecipe f(tt::TruthTable) = return pretty_table(LaTeXString, tt)

"""
    formatter(t::Type{Latexify.LaTeXString})

| `t`                    | `formatter(t)(⊤, _, _)` | `formatter(t)(⊥, _, _)` |
| :--------------------- | :---------------------- | :---------------------- |
| `Latexify.LaTeXString` | `"\$\\top\$"`           | `"\$\\bot\$"`           |
"""
formatter(::Type{LaTeXString}) = (v, _, _) -> string(latexify(v ? "⊤" : "⊥"))

__pretty_table(backend::Val{:latex}, io, tt; formatters = formatter(LaTeXString), kwargs...) =
    ___pretty_table(backend, io, tt.body; header = map(latexify ∘ Symbol, tt.header), formatters, kwargs...)

"""
    pretty_table(
        ::Latexify.LaTexString, x::Union{Proposition, TruthTable};
        backend = Val(:latex), kwargs...
    )

Equivalent to [`Latexify.LaTeXString(pretty_table(String, x; backend, kwargs...))`](@ref pretty_table).

See also [`Proposition`](@ref) and [`TruthTable`](@ref).

# Examples
```jldoctest
julia> pretty_table(Latexify.LaTeXString, @atomize p ∧ q)
L"\\begin{tabular}{|l|l|l|}
  \\hline
  \\textbf{\$p\$} & \\textbf{\$q\$} & \\textbf{\$p \\wedge q\$} \\\\\\hline
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
