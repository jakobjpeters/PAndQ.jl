
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

function make_logo(directory)
    Drawing(julia_dots.width, julia_dots.height, :svg, directory * "logo.svg")
    placeimage(julia_dots)

    fontsize(128)
    fontface("JuliaMono")

    for (character, (x, y)) in zip(("p", "∧", "q"), map(
        i -> (julia_dots.width * i / 4, julia_dots.height * (iseven(i) ? 1 : 5) / 8),
        1:3
    ))
        text(character, x, y; :halign => :center, :valign => :top)
    end

    finish()
end
