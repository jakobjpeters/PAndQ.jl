
using Base: get_extension
using Documenter: DocMeta.setdocmeta!, doctest
using Latexify
using Markdown
using PAndQ
using PAndQ: PicoSAT
using Test: @testset, @test, detect_ambiguities, detect_unbound_args

@testset "`detect_ambiguities` and `detect_unbound_args`" all(
    detect -> isempty(detect(PAndQ)), (detect_ambiguities, detect_unbound_args))

@testset "`doctest`" begin
    setdocmeta!(PAndQ, :DocTestSetup, :(using PAndQ))
    setdocmeta!(PicoSAT, :DocTestSetup, :(using PAndQ))

    doctest(PAndQ)

    for extension in map(
        extension -> chop(extension; tail = 3),
        cd(readdir, joinpath(@__DIR__, "..", "ext"))
    )
        _module = get_extension(PAndQ, Symbol(extension))

        setdocmeta!(_module, :DocTestSetup,
            :(using PAndQ, $(Symbol(chop(extension; tail = 9)))))

        doctest(_module)
    end
end
