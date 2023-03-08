
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
        "Tutorial" => "tutorial.md",
        map(["manual", "internals"]) do chapter
            uppercasefirst(chapter) => map(
                ["operators", "propositions", "printing", "utility", "semantics"]
            ) do page
                uppercasefirst(page) => chapter * "/" * page * ".md"
            end
        end...
    ],
    strict = true
)

deploydocs(
    repo = "github.com/jakobjpeters/PAQ.jl.git",
)
