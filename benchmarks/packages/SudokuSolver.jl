
module SudokuSolverBenchmarks

using SudokuSolver: naivesolver!, prioritysolver!, reordersolver!, solvesudoku

sudoku(solver) = solvesudoku(zeros(Int, 9, 9); solver)

sudoku_naive() = sudoku(naivesolver!)
sudoku_reorder() = sudoku(reordersolver!)
sudoku_priority() = sudoku(prioritysolver!)

const functions = [sudoku_naive, sudoku_reorder, sudoku_priority]

end # SudokuSolverBenchmarks
