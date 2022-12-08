
using Luxor

#=
Copyright (c) 2012-2019: Stefan Karpinski <stefan@karpinski.org>

License:
 - https://github.com/JuliaLang/julia-logo-graphics/blob/master/LICENSE.md

Modifications:
 - `P ∧ Q` overlay
=#
const julia_dots_url = "https://raw.githubusercontent.com/JuliaLang/julia-logo-graphics/b5551ca7946b4a25746c045c15fbb8806610f8d0/images/julia-dots.svg"
const julia_dots = readsvg(download(julia_dots_url))

function make_logo(DIR)
    mkpath(DIR * "/src/assets")

    Drawing(julia_dots.width, julia_dots.height, :svg, DIR * "/src/assets/logo.svg")
    placeimage(julia_dots)

    fontface("JuliaMono")
    fontsize(128)

    align = (:halign => :center, :valign => :top)
    p = Point(julia_dots.width/4, 5*julia_dots.height/8)
    text("p", p; align...)
    p = p + Point(julia_dots.width/2, 0)
    text("q", p; align...)
    p = Point(julia_dots.width/2, julia_dots.height/8)
    text("∧", p; align...)

    finish()
end