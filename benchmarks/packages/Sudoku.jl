
module SudokuBenchmarks

using Sudoku

sudoku_constraints() = sudoku(zeros(Int, 9, 9))

const functions = [sudoku_constraints]

end # SudokuBenchmarks
