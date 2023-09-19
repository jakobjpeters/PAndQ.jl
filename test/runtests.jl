
using Test: @testset, @test, detect_ambiguities, detect_unbound_args
using Documenter: DocMeta.setdocmeta!, doctest
using Base: get_extension
using PAndQ
using Markdown
using Latexify

@testset "`detect_ambiguities` and `detect_unbound_args`" all(
    detect -> isempty(detect(PAndQ)), (detect_ambiguities, detect_unbound_args)
)

@testset "`doctest`" begin
    setdocmeta!(
        PAndQ,
        :DocTestSetup,
        :(using PAndQ)
    )

    doctest(PAndQ)

    for extension in map(
        extension -> extension[begin:end - 3],
        cd(readdir, joinpath(@__DIR__, "..", "ext"))
    )
        _module = get_extension(PAndQ, Symbol(extension))

        setdocmeta!(
            _module,
            :DocTestSetup,
            :(using PAndQ, $(Symbol(extension[begin:end - 9])))
        )

        doctest(_module)
    end
end
