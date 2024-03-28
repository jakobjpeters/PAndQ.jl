
one_to_nine(grid) = Set(grid) == Set(1:9)

setup(grid -> all([
    all(one_to_nine, eachrow(grid)),
    all(one_to_nine, eachcol(grid)),
    all(i -> all(j -> one_to_nine(grid[i:i + 2, j:j + 2]), 1:3:7), 1:3:7)
]), "sudoku", map(_module -> _module => _module.functions, modules))
