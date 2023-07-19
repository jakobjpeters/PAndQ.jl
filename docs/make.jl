
using Documenter
using PAQ

const directory = (@__DIR__) * "/src/assets/"
if !ispath(directory * "logo.svg")
    include("logo.jl")
    make_logo(directory)
end

DocMeta.setdocmeta!(
    PAQ,
    :DocTestSetup,
    :(using PAQ),
    recursive = true,
)

makedocs(
    sitename = "PAQ.jl",
    format = Documenter.HTML(),
    modules = [PAQ],
    pages = [
        "Home" => "index.md",
        "Tutorial" => "tutorial.md",
        "Manual" => map(
            ["operators", "propositions", "printing", "utility", "semantics", "extensions"]
        ) do name
            name |> titlecase => "manual/" * name * ".md"
        end,
        "Internals" => "internals.md"
    ],
    strict = true
)

deploydocs(repo = "github.com/jakobjpeters/PAQ.jl.git")
