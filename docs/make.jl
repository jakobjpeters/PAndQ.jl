
using Documenter
using Logic
import Logic: Valuation, Boolean, And, Not

if !ispath("assets/Logic.svg")
    include("logo.jl")
    make_logo()
end

DocMeta.setdocmeta!(Logic, :DocTestSetup,
    :(using Logic; import Logic: Valuation, Boolean, And, Not);
recursive=true)

makedocs(
    sitename = "Logic.jl",
    format = Documenter.HTML(),
    modules = [Logic],
    pages = [
        "index.md",
        "propositional.md",
        "internals.md"
    ]
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
#=deploydocs(
    repo = "<repository url>"
)=#
