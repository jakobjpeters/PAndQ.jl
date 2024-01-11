
# Sudoku

This tutorial will demonstrate how to solve a Sudoku puzzle by encoding the rules sudoku into propositional logic. Although it is more computationally efficient to frame Sudoku as an optimization problem, it is a good example of a non-trivial logic problem.

## Creating a Grid

Sudoku is traditionally played on a 9x9 grid. Such a grid can be created using the [`pretty_table`](@ref) function.

```@repl 1
using PAndQ
lines = collect(0:3:9);
grid = zeros(Int, 9, 9);
print_grid(grid) = pretty_table(
    map(cell -> cell == 0 ? "⋅" : string(cell), grid);
    vlines = lines, hlines = lines, show_header = false
);
print_grid(grid)
```

## Encoding the Rules

The grid is currently empty. Placing random numbers in some of the cells has a very high probability of generating an unsolvable board. Knowing which numbers to place in each cell requires the same functionality that it takes to solve the board in the first place. Encoding the rules of Sudoku into a proposition means that any solution to that proposition can be decoded into a solution of Sudoku.

Given the predicate `P(row, column, number)` is true when the given `row` and `column` of the grid contains the given `number`:

```@repl 1
p(row, column, number) = @atomize $((row, column) => number);
```

1\. Each row contains each number from `1` to `9`. This proposition can be read as "for each row and for each number, one of the cells in that row contains that number".

```math
\bigwedge\limits_{i = 1}^9 \bigwedge\limits_{n = 1}^9 \bigvee\limits_{j = 1}^9 P(i, j, n) \\
```

```@repl 1
one = fold((∧) => 1:9, (∧) => 1:9, (∨) => 1:9) do i, n, j
    p(i, j, n)
end;
```

2\. Each column contains each number from `1` to `9`. This proposition can be read as "for each column and for each number, one of the cells of that column contains that number".

```math
\bigwedge\limits_{j = 1}^9 \bigwedge\limits_{n = 1}^9 \bigvee\limits_{i = 1}^9 P(i, j, n) \\
```

```@repl 1
two = fold((∧) => 1:9, (∧) => 1:9, (∨) => 1:9) do j, n, i
    p(i, j, n)
end;
```

3\. Each 3x3 subgrid contains each number from `1` to `9`. This proposition can be read as "for each 3x3 subgrid and for each number, one of the cells of that subgrid contains that number".

```math
\bigwedge\limits_{r = 0}^2 \bigwedge\limits_{c = 0}^2 \bigwedge\limits_{n = 1}^9 \bigvee\limits_{i = 1}^3 \bigvee\limits_{j = 1}^3 P(3r + i, 3c + j, n) \\
```

```@repl 1
three = fold(
    (∧) => 0:2, (∧) => 0:2, (∧) => 1:9, (∨) => 1:3, (∨) => 1:3
) do r, c, n, i, j
    p(3r + i, 3c + j, n)
end;
```

4\. Each cell contains a single number. This proposition can be read as "for each cell and for pair of unique numbers, that cell does not contain both numbers".

```math
\bigwedge\limits_{i = 1}^9 \bigwedge\limits_{j = 1}^9 \bigwedge\limits_{n = 1}^8 \bigwedge\limits_{m = n + 1}^9 ¬P(i, j, n) ∨ ¬P(i, j, m)
```

```@repl 1
four = fold((∧) => 1:9, (∧) => 1:9, (∧) => 1:8) do i, j, n
    map_folds((∧) => n + 1:9) do m
        ¬p(i, j, n) ∨ ¬p(i, j, m)
    end
end;
```

The [`conjunction`](@ref) of these rules represent the encoding.

```@repl 1
rules = ⋀((one, two, three, four));
```

!!! note
    See also [`@atomize`](@ref), [`fold`](@ref), [`conjunction`](@ref), [`and`](@ref), and [`or`](@ref).

## Finding a Solution

Given a proposition, [`valuations`](@ref) that result in a true interpretation can be found using the [`solutions`](@ref) function. Since the encoding does not specify any initial values of the cells, there will be many possible solutions to the proposition. Each solution contains 729 assignments of a [`Constant`](@ref PAndQ.Constant)s to a `Bool`, which makes sense because the 9x9 grid has 81 cells and each cell contains one of 9 possible numbers.

```@repl 1
first_solution(p) = collect(first(solutions(p)));
solution = first_solution(rules)
```

The assignments from a `Constant` to `true` correspond to cells that contain the given numbers. There are 81 such constants, with each [`value`](@ref) corresponding to a cell in the grid.

```@repl 1
extract(solution) = map(something ∘ value ∘ first, filter(last, solution));
cells = extract(solution)
```

Each cell is a `Pair` in the form `(row, column) => number`, which maps from the row and column of the grid to its corresponding number.

```@repl 1
function decode!(grid, cells)
    for ((row, column), value) in cells
        grid[row, column] = value
    end

    grid
end;
print_grid(decode!(grid, cells))
```

## Creating a Puzzle

Now that a solution has been found, it can be used to create a puzzle by removing some of the known values.

```@repl 1
print_grid(grid .*= rand(Bool, 9, 9))
```

Since the `rules` represent an empty Sudoku grid, finding a solution to this puzzle means encoding the initial values as additional rules and finding a solution to the combined ruleset. If a grid has no solutions, then it contains a contradiction to the rules.

```@repl 1
print_grid(decode!(grid, extract(first_solution(rules ∧ ⋀(map(
    i -> p(i.I..., grid[i]), filter(i -> grid[i] != 0, CartesianIndices(grid))))))))
```
