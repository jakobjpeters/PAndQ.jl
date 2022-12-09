
using Test
using Documenter
using PAQ

DocMeta.setdocmeta!(
    PAQ,
    :DocTestSetup,
    :(
        using PAQ;
        @primitive p q
    ),
    recursive=true
)

@testset "All" begin
    doctest(PAQ)
end
