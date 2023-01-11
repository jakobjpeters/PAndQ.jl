
using Documenter
using PAQ

const DIR = @__DIR__
if !ispath(DIR * "/src/assets/logo.svg")
    include("logo.jl")
    make_logo(DIR)
end

DocMeta.setdocmeta!(
    PAQ,
    :DocTestSetup,
    :(
        using PAQ;
        @primitive p q
    ),
    recursive = true,
)

makedocs(
    sitename = "PAQ.jl",
    format = Documenter.HTML(),
    modules = [PAQ],
    pages = [
        "Home" => "index.md",
        "Tutorial" => "tutorial.md",
        "Manual" => [
            "Propositional Logic" => "manual/propositional_logic.md",
            "Boolean Operators" => "manual/boolean_operators.md",
            "Semantics" => "manual/semantics.md",
            "Pretty Printing" => "manual/pretty_printing.md",
            "Internals" => "manual/internals.md",
        ]
    ],
    strict = true
)

deploydocs(
    repo = "github.com/jakobjpeters/PAQ.jl.git",
)
