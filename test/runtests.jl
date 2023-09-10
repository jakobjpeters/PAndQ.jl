
using Test: @testset, @test, detect_ambiguities, detect_unbound_args
using Documenter: DocMeta.setdocmeta!, doctest
using PAndQ

@testset "`detect_ambiguities` and `detect_unbound_args`" begin
    isempty(detect_ambiguities(PAndQ, recursive = true))
    isempty(detect_unbound_args(PAndQ, recursive = true))
end

setdocmeta!(
    PAndQ,
    :DocTestSetup,
    :(using PAndQ),
    recursive = true
)

@testset "`doctest`" doctest(PAndQ)
