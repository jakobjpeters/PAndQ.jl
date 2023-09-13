
using Test: @testset, @test, detect_ambiguities, detect_unbound_args
using Documenter: DocMeta.setdocmeta!, doctest
using Markdown
using Latexify
using PAndQ

@testset "`detect_ambiguities` and `detect_unbound_args`" all(
    detect -> isempty(detect(PAndQ)), (detect_ambiguities, detect_unbound_args)
)

setdocmeta!(
    PAndQ,
    :DocTestSetup,
    :(using PAndQ),
)

@testset "`doctest`" doctest(PAndQ)
