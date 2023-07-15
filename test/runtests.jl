
using Test
using Documenter
using PAQ

@testset "`detect_ambiguities` and `detect_unbound_args`" begin
    foreach([:detect_ambiguities, :detect_unbound_args]) do detect
        @eval @test $detect(PAQ, recursive = true) |> isempty
    end
end

DocMeta.setdocmeta!(
    PAQ,
    :DocTestSetup,
    :(using PAQ),
    recursive=true
)

@testset "`doctest`" PAQ |> doctest
