
module SimpleSudokuBenchmarks

using SimpleSudoku

sudoku_recursion() = first(solve_sudoku(zeros(Int, 9, 9)))

const functions = [sudoku_recursion]

end # SimpleSudokuBenchmarks
