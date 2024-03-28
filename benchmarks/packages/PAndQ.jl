
module PAndQBenchmarks

using PAndQ

function sudoku(solver)
    grid = zeros(Int, 9, 9)
    p(row, column, number) = @atomize $((row, column) => number)

    cells, assignments = solutions(⋀(
        fold((i, n, j) -> p(i, j, n), (∧) => 1:9, (∧) => 1:9, (∨) => 1:9),
        fold((j, n, i) -> p(i, j, n), (∧) => 1:9, (∧) => 1:9, (∨) => 1:9),
        fold((r, c, n, i, j) -> p(3r + i, 3c + j, n), (∧) => 0:2, (∧) => 0:2, (∧) => 1:9, (∨) => 1:3, (∨) => 1:3),
        fold((i, j, n) -> fold(m -> ¬p(i, j, n) ∨ ¬p(i, j, m), (∧) => n + 1:9), (∧) => 1:9, (∧) => 1:9, (∧) => 1:8)
    ); solver)

    for (cell, assignment) in zip(cells, first(assignments)) if assignment
        (row, column), number = something(value(Pair{NTuple{2, Int}, Int}, cell))
        grid[row, column] = number
    end end

    grid
end

sudoku_Z3() = sudoku(Z3)
sudoku_PicoSAT() = sudoku(PicoSAT)

const functions = [sudoku_Z3, sudoku_PicoSAT]

end # PAndQBenchmarks
