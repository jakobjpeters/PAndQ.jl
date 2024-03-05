
module LatexifyExtension

import PAndQ: formatter, _print_table
using Latexify: Latexify, LaTeXString, @latexrecipe, latexify
using PAndQ: Operator, Proposition, TruthTable, symbol, __print_table, print_table

@latexrecipe f(x::Union{Operator, Proposition}) = return Symbol(repr(MIME"text/plain"(), x))
@latexrecipe f(t::TruthTable) = return LaTeXString(sprint(io -> print_table(io, t; backend = Val(:latex))))

"""
    formatter(::Latexify.LaTeXString)

# Examples
```jldoctest
julia> formatter(Latexify.LaTeXString)(true, nothing, nothing)
L"\$\\top\$"

julia> formatter(Latexify.LaTeXString)(false, nothing, nothing)
L"\$\\bot\$"
```
"""
formatter(::Type{LaTeXString}) = (v, _, _) -> string(latexify(v ? "⊤" : "⊥"))

_print_table(backend::Val{:latex}, io, t; formatters = formatter(LaTeXString), kwargs...) =
    __print_table(backend, io, t.body; header = map(latexify ∘ Symbol, t.header), formatters, kwargs...)

end # module
