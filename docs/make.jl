
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
    :(using PAQ;),
    recursive = true,
)

makedocs(
    sitename = "PAQ.jl",
    format = Documenter.HTML(),
    modules = [PAQ],
    pages = [
        "Home" => "index.md",
        # "Tutorial" => "tutorial.md",
        "Manual" => [
            "Operators" => "manual/operators.md",
            "Types" => "manual/types.md",
            "Pretty Printing" => "manual/pretty_printing.md",
            "Utility" => "manual/utility.md",
            "Semantics" => "manual/semantics.md",
        ]
    ],
    strict = true
)

deploydocs(
    repo = "github.com/jakobjpeters/PAQ.jl.git",
)
