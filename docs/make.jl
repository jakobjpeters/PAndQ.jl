
using Documenter
using PAndQ

const directory = (@__DIR__) * "/src/assets/"
if !ispath(directory * "logo.svg")
    include("logo.jl")
    make_logo(directory)
end

DocMeta.setdocmeta!(
    PAndQ,
    :DocTestSetup,
    :(using PAndQ),
    recursive = true,
)

makedocs(
    sitename = "PAndQ.jl",
    format = Documenter.HTML(),
    modules = [PAndQ],
    pages = [
        "Home" => "index.md",
        "Tutorial" => "tutorial.md",
        "Manual" => map(
            name -> titlecase(name) => "manual/" * name * ".md",
            ["operators", "propositions", "printing", "utility", "semantics", "extensions"]
        ),
        "Internals" => "internals.md"
    ],
    strict = true
)

deploydocs(repo = "github.com/jakobjpeters/PAndQ.jl.git")
