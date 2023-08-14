
using Documenter: DocMeta.setdocmeta!, makedocs, HTML, deploydocs
using PAndQ

const directory = (@__DIR__) * "/src/assets/"
if !ispath(directory * "logo.svg")
    mkpath(directory)
    include("logo.jl")
    make_logo(directory)
end

setdocmeta!(
    PAndQ,
    :DocTestSetup,
    :(using PAndQ),
    recursive = true,
)

makedocs(
    sitename = "PAndQ.jl",
    format = HTML(edit_link = "main"),
    modules = [PAndQ],
    pages = [
        "Home" => "index.md",
        "Tutorial" => "tutorial.md",
        "Manual" => map(
            name -> titlecase(name) => "manual/" * name * ".md",
            ["operators", "propositions", "printing", "semantics", "extensions"]
        ),
        "Internals" => "internals.md"
    ],
    strict = true
)

deploydocs(
    repo = "github.com/jakobjpeters/PAndQ.jl.git",
    devbranch = "main"
)
