
using Test
using Documenter
using PAQ

@test isempty(detect_ambiguities(PAQ, recursive = true))
@test isempty(detect_unbound_args(PAQ, recursive = true))

DocMeta.setdocmeta!(
    PAQ,
    :DocTestSetup,
    :(using PAQ),
    recursive=true
)

@testset "All" begin
    doctest(PAQ)
end
