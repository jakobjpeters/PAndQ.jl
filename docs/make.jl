
using Documenter: DocMeta.setdocmeta!, makedocs, HTML, deploydocs
using Base: get_extension
using PAndQ
using Markdown
using Latexify

const directory = joinpath(@__DIR__, "src", "assets")
if !ispath(joinpath(directory, "logo.svg"))
    mkpath(directory)
    include("logo.jl")
    make_logo(directory)
end

setdocmeta!(
    PAndQ,
    :DocTestSetup,
    :(using PAndQ)
)

const extensions = map(
    extension -> extension[begin:end - 3],
    cd(readdir, joinpath(@__DIR__, "..", "ext"))
)
const modules = [PAndQ; map(
    extension -> get_extension(PAndQ, Symbol(extension)),
extensions)]

for (extension, _module) in zip(extensions, modules[2:end])
    setdocmeta!(
        _module,
        :DocTestSetup,
        :(using PAndQ, $(Symbol(extension[begin:end - 9])))
    )
end

makedocs(;
    modules,
    strict = true,
    format = HTML(edit_link = "main"),
    sitename = "PAndQ.jl",
    pages = [
        "Home" => "index.md",
        "Tutorial" => "tutorial.md",
        "Manual" => map(
            name -> titlecase(name) => "manual/" * name * ".md",
            ["operators", "propositions", "printing", "semantics"]
        ),
        "Extensions" => "extensions.md",
        "Internals" => "internals.md"
    ]
)

deploydocs(
    repo = "github.com/jakobjpeters/PAndQ.jl.git",
    devbranch = "main"
)
