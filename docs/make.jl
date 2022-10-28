
using Documenter
using Logic
import Logic: Valuation, Boolean, And, Not

DocMeta.setdocmeta!(Logic, :DocTestSetup,
    :(using Logic; import Logic: Valuation, Boolean, And, Not);
recursive=true)

makedocs(
    sitename = "Logic",
    format = Documenter.HTML(),
    modules = [Logic]
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
#=deploydocs(
    repo = "<repository url>"
)=#
