
using Documenter
using PAQ
import PAQ: Valuation, Boolean, And, Not

if !ispath("assets/logo.svg")
    include("logo.jl")
    make_logo()
end

DocMeta.setdocmeta!(PAQ, :DocTestSetup,
    :(using PAQ; import PAQ: Valuation, Boolean, And, Not);
recursive=true)

makedocs(
    sitename = "Logic.jl",
    format = Documenter.HTML(),
    modules = [PAQ],
    pages = [
        "Home" => "index.md",
        "Logic" => "PAQ.md",
        "Internals" => "internals.md"
    ]
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
#=deploydocs(
    repo = "<repository url>"
)=#
