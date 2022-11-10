
using Documenter
using PAQ

if !ispath("assets/logo.svg")
    include("logo.jl")
    make_logo()
end

DocMeta.setdocmeta!(PAQ, :DocTestSetup,
    :(using PAQ; import PAQ: Valuation, Boolean, And, Not);
recursive=true)

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

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
#=deploydocs(
    repo = "<repository url>"
)=#
