
using Base: get_extension
using Documenter: DocMeta.setdocmeta!, HTML, deploydocs, makedocs
using Latexify
using Markdown
using PAndQ
using PAndQ: PicoSAT

const directory = joinpath(@__DIR__, "src", "assets")
const logo = joinpath(directory, "logo.svg")
if !ispath(logo)
    using Luxor: readsvg, Drawing, placeimage, fontface, fontsize, text, Point, finish

    #=
    `julia-dots.svg`
    Copyright (c) 2012-2022: Stefan Karpinski <stefan@karpinski.org>

    License
    https://github.com/JuliaLang/julia-logo-graphics/blob/master/LICENSE.md

    Modifications
    `P ∧ Q` overlay
    =#
    const julia_dots = readsvg(download(
        "https://raw.githubusercontent.com/JuliaLang/julia-logo-graphics/b5551ca7946b4a25746c045c15fbb8806610f8d0/images/julia-dots.svg"
    ))

    mkpath(directory)
    Drawing(julia_dots.width, julia_dots.height, :svg, logo)
    placeimage(julia_dots)

    fontsize(128)
    fontface("JuliaMono")

    for (character, (x, y)) in zip(("p", "∧", "q"), map(
        i -> (julia_dots.width * i / 4, julia_dots.height * (iseven(i) ? 1 : 5) / 8),
    1:3))
        text(character, x, y; :halign => :center, :valign => :top)
    end

    finish()
end

const modules = [PicoSAT, Interface, PAndQ]

setdocmeta!(PAndQ, :DocTestSetup, :(using PAndQ); recursive = true)

for extension in map(extension -> chop(extension; tail = 3), cd(readdir, joinpath(@__DIR__, "..", "ext")))
    _module = get_extension(PAndQ, Symbol(extension))
    setdocmeta!(_module, :DocTestSetup, :(using PAndQ, $(Symbol(chop(extension; tail = 9)))))
    push!(modules, _module)
end

makedocs(;
    modules,
    format = HTML(edit_link = "main"),
    sitename = "PAndQ.jl",
    pages = [
        "Home" => "index.md",
        "Getting Started" => "getting_started.md",
        "Tutorials" => ["tutorials/sudoku.md", "tutorials/custom_operators"],
        "Manual" => map(
            name -> titlecase(name) => "manual/" * name * ".md",
            ["interface", "operators", "propositions", "semantics", "printing", "extensions", "internals"]
        ),
    ]
)

deploydocs(
    repo = "github.com/jakobjpeters/PAndQ.jl.git",
    devbranch = "main"
)
