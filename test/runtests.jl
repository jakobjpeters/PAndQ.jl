
using Test
using Documenter
using PAQ

foreach([:detect_ambiguities, :detect_unbound_args]) do f
    @eval @test isempty($f(PAQ, recursive = true))
end

DocMeta.setdocmeta!(
    PAQ,
    :DocTestSetup,
    :(using PAQ),
    recursive=true
)

@testset "All" begin
    doctest(PAQ)
end
