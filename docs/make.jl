
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
    recursive=true
)

makedocs(
    sitename = "PAQ.jl",
    format = Documenter.HTML(),
    modules = [PAQ],
    pages = [
        "Home" => "index.md",
        "Tutorial" => "tutorial.md",
        "Manual" => [
            "Types" => "types.md",
            "Boolean Operators" => "boolean_operators.md",
            "Utility" => "utility.md"
        ]
    ]
)

deploydocs(
    repo = "github.com/jakobjpeters/PAQ.jl.git",
)
