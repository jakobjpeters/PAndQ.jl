
using Test
using Documenter
using PAndQ

@testset "`detect_ambiguities` and `detect_unbound_args`" begin
    foreach([:detect_ambiguities, :detect_unbound_args]) do detect
        @eval @test $detect(PAndQ, recursive = true) |> isempty
    end
end

DocMeta.setdocmeta!(
    PAndQ,
    :DocTestSetup,
    :(using PAndQ),
    recursive=true
)

@testset "`doctest`" PAndQ |> doctest
