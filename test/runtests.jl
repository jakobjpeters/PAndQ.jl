
using Test
using Documenter
using PAQ

DocMeta.setdocmeta!(
    PAQ,
    :DocTestSetup,
    :(using PAQ),
    recursive=true
)

@testset "All" begin
    doctest(PAQ)
end
