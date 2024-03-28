
module JulogBenchmarks

using Julog

"""
sudoku(Rows) :-
    length(Rows, 9),
    maplist(same_length(Rows), Rows),
    append(Rows, Vs), Vs ins 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
    blocks(As, Bs, Cs),
    blocks(Ds, Es, Fs),
    blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    blocks(Ns1, Ns2, Ns3).
"""

"""
@julog [
    length(Rows, 9) <<= true,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    # @julog(Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is]),
    Rows <<= [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
    blocks(As, Bs, Cs),
    blocks(Ds, Es, Fs),
    blocks(Gs, Hs, Is) <<= true
]
"""

"""
sudoku(Rows) <<= 
 <<= true
9,
    maplist(all_distinct, Rows),
        transpose(Rows, Columns),
    maplist(all_distinct, Columns),
        Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
            blocks(As, Bs, Cs),
    blocks(Ds, Es, Fs),
    blocks(Gs, Hs, Is) <<= true
blocks([], [], []) <<= true
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) <<= """

function sudoku()
    grid = zeros(Int, 9, 9)
    p(row, column, number) = @julog $((row, column) => number)

    # and = (ps...) -> Julog.Compound(:and, [ps...])
    # or = (ps...) -> Julog.Compound(:or, [ps...])

    # goals = Iterators.flatmap(i -> Iterators.flatmap(j -> map(n -> p(i, j, n), 1:9), 1:9), 1:9)

    # clause = and(
    #     mapreduce(and, 1:9) do i
    #         mapreduce(and, 1:9) do n
    #             mapreduce(or, 1:9) do j
    #                 p(i, j, n)
    #             end
    #         end
    #     end,
    #     mapreduce(and, 1:9) do j
    #         mapreduce(and, 1:9) do n
    #             mapreduce(or, 1:9) do i
    #                 p(i, j, n)
    #             end
    #         end
    #     end,
    #     mapreduce(and, 1:2) do r
    #         mapreduce(and, 1:2) do c
    #             mapreduce(and, 1:9) do n
    #                 mapreduce(or, 1:3) do i
    #                     mapreduce(or, 1:3) do j
    #                         p(3r + i, 3c + j, n)
    #                     end
    #                 end
    #             end
    #         end
    #     end,
    #     mapreduce(and, 1:9) do i
    #         mapreduce(and, 1:9) do j
    #             mapreduce(and, 1:8) do n
    #                 mapreduce(and, n + 1:9) do m
    #                     @julog or(not($(p(i, j, n))), not($(p(i, j, m))))
    #                 end
    #             end
    #         end
    #     end
    # )

    # resolve(goals, [clause])

    # cells, assignments = solutions(⋀(
    #     fold((i, n, j) -> p(i, j, n), (∧) => 1:9, (∧) => 1:9, (∨) => 1:9),
    #     fold((j, n, i) -> p(i, j, n), (∧) => 1:9, (∧) => 1:9, (∨) => 1:9),
    #     fold((r, c, n, i, j) -> p(3r + i, 3c + j, n), (∧) => 0:2, (∧) => 0:2, (∧) => 1:9, (∨) => 1:3, (∨) => 1:3),
    #     fold((i, j, n) -> fold(m -> ¬p(i, j, n) ∨ ¬p(i, j, m), (∧) => n + 1:9), (∧) => 1:9, (∧) => 1:9, (∧) => 1:8)
    # ); solver)

    # for (cell, assignment) in zip(cells, first(assignments)) if assignment
    #     (row, column), number = something(value(Pair{NTuple{2, Int}, Int}, cell))
    #     grid[row, column] = number
    # end end

    # grid
end

const functions = [sudoku]

end # JulogBenchmarks
