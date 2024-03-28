
module Benchmarks

using BenchmarkTools: BenchmarkGroup, @benchmarkable, @tagged, median, run
using Base.Iterators: flatten
using Plots: bar, savefig

const benchmark_group = BenchmarkGroup()
const packages = map(package -> package * ".jl", ["PAndQ", "JuMP", "Sudoku", "SimpleSudoku", "SudokuSolver"])

for package in packages
    include(joinpath("packages", package))
end

const modules = [
    PAndQBenchmarks,
    JuMPBenchmarks,
    SudokuBenchmarks,
    SimpleSudokuBenchmarks,
    SudokuSolverBenchmarks
]

setup(verify, tag, module_functions) =
    for (_module, functions) in module_functions
        package = replace(string(_module), "Main" => "", "Benchmarks" => "", "." => "") * ".jl"

        for f in functions
            verify(f()) ?
                benchmark_group[(package, f)] =
                    BenchmarkGroup([package, tag], replace(string(f), tag * "_" => "") => @benchmarkable $f()) :
                error("`Incorrect solution from $_module.$f`")
        end
    end

include("sudoku.jl")

function benchmark()
    trials = run(benchmark_group; samples = 10, seconds = typemax(Int))
    benchmarks = map(package -> package => minimum(last, map(((label, trial),) -> replace(label, "_" => " ") => median(trial).time / 1_000_000_000,
        map(only ∘ last, trials[@tagged package]))), packages)

    xs, ys = foldl(((xs, ys), (x, y)) -> (push!(xs, x), push!(ys, y)), append!([benchmarks[1]],
        sort!(benchmarks[2:end]; by = last)); init = (String[], Float64[]))

    bar(xs, ys;
        ylabel = "seconds",
        plot_title = "Sudoku",
        legend = :none,
        grid = :y,
        yticks = 0:0.1:10,
        ylims = (0, ceil(maximum(ys)))
    )
end
benchmark(file) = savefig(benchmark(), file)

end # Benchmarks
