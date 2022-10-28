
using Luxor

function make_logo()
    mkpath("src/assets")

    dots_address = "https://raw.githubusercontent.com/JuliaLang/julia-logo-graphics/b5551ca7946b4a25746c045c15fbb8806610f8d0/images/julia-dots.svg"
    dots_svg = download(dots_address)
    dots = readsvg(dots_svg)
    Drawing(dots.width, dots.height, :svg, "src/assets/logo.svg")
    placeimage(dots)

    fontface("JuliaMono")
    fontsize(128)

    align = (:halign => :center, :valign => :top)
    p = Point(dots.width/4, 5*dots.height/8)
    text("p", p; align...)
    p = p + Point(dots.width/2, 0)
    text("q", p; align...)
    p = Point(dots.width/2, dots.height/8)
    text("∧", p; align...)

    finish()
end