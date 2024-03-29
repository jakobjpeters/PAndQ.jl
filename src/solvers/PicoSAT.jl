
"""
    PicoSAT

This module provides an interface to libpicosat_jll.jl.
"""
module PicoSAT

import Base: IteratorSize, eltype, isdone, iterate
using Base: SizeUnknown
using Base.Libc: FILE, RawFD
using libpicosat_jll: libpicosat

# Library

"""
    picosat_init()

Construct a new PicoSAT instance and return a pointer to it.
"""
picosat_init() = @ccall libpicosat.picosat_init()::Ptr{Cvoid}

"""
    picosat_reset(pico_sat)

Destruct the `pico_sat` instance.
"""
picosat_reset(pico_sat) = @ccall libpicosat.picosat_reset(pico_sat::Ptr{Cvoid})::Cvoid

"""
    picosat_adjust
"""
picosat_adjust(pico_sat, n) =
    @ccall libpicosat.picosat_adjust(pico_sat::Ptr{Cvoid}, n::Cint)::Cvoid

"""
    picosat_add(pico_sat, literal)

Append the `literal` to the `pico_sat` instance's current disjunctive clause.
"""
picosat_add(pico_sat, literal) =
    @ccall libpicosat.picosat_add(pico_sat::Ptr{Cvoid}, literal::Cint)::Cint

"""
    picosat_variables(pico_sat)

Return the number of unique atoms in the `pico_sat` instance.
"""
picosat_variables(pico_sat) =
    @ccall libpicosat.picosat_variables(pico_sat::Ptr{Cvoid})::Cint

"""
    picosat_print(pico_sat, file)

Write the DIMACS format of the `pico_sat` instance's proposition to the given `file`.
"""
picosat_print(pico_sat, file) =
    @ccall libpicosat.picosat_print(pico_sat::Ptr{Cvoid}, file::Ptr{Cvoid})::Cvoid

"""
    picosat_sat(pico_sat, limit)

Search for a satisfiable assignment of the `pico_sat` instance's proposition and return
`0`, `10`, or `20` if the status is unknown, satisfiable, or unsatisfiable, respectively.
"""
picosat_sat(pico_sat, limit) =
    @ccall libpicosat.picosat_sat(pico_sat::Ptr{Cvoid}, limit::Cint)::Cint

"""
    picosat_deref(pico_sat, atom)

Return the assignment of the `atom`, where `1`, `-1`, and `0`
indicate `true`, `false`, and unknown, respectively.

This function must be called after verifying that the status of [`picosat_sat`](@ref) is satisfiable.
"""
picosat_deref(pico_sat, atom) =
    @ccall libpicosat.picosat_deref(pico_sat::Ptr{Cvoid}, atom::Cint)::Cint

# Utilities

"""
    add_clause(pico_sat, clause)

Mutate the `pico_sat` instance's proposition to be the
conjunction of itself and the disjunctive `clause`.
"""
function add_clause(pico_sat, clause)
    for literal in clause
        picosat_add(pico_sat, literal)
    end

    picosat_add(pico_sat, 0)
end

"""
    initialize(clauses, n)

Return a PicoSAT pointer with its proposition being a conjunction of the disjunctive `clauses`.
"""
function initialize(clauses, n)
    pico_sat = picosat_init()
    picosat_adjust(pico_sat, n)

    for clause in clauses
        add_clause(pico_sat, clause)
    end

    pico_sat
end

"""
    Solutions
    Solutions(clauses)

A stateful iterator of valuations that satisfy the given proposition.

Calling `finalize` on this iterator will first check whether it has already been finalized.
If not, it will call [`picosat_reset`](@ref) on its PicoSAT pointer and set the pointer equal to `C_NULL`.
"""
mutable struct Solutions
    pico_sat::Ptr{Cvoid}

    Solutions(clauses, n) = finalizer(new(initialize(clauses, n))) do solutions
        pico_sat = solutions.pico_sat
        pico_sat == C_NULL && return
        solutions.pico_sat = C_NULL
        picosat_reset(pico_sat)
    end
end

"""
    eltype(::Type{Solutions})

The type of the elements generated by a [`Solutions`](@ref PicoSAT.Solutions) iterator.

# Examples
```jldoctest
julia> eltype(PAndQ.PicoSAT.Solutions)
Vector{Bool} (alias for Array{Bool, 1})
```
"""
eltype(::Type{Solutions}) = Vector{Bool}

"""
    IteratorSize(::Type{Solutions})

Since counting the number of [`Solutions`](@ref) to a proposition is intractable,
its `IteratorSize` is `Base.SizeUnknown`.

# Examples
```jldoctest
julia> Base.IteratorSize(PAndQ.PicoSAT.Solutions)
Base.SizeUnknown()
```
"""
IteratorSize(::Type{Solutions}) = SizeUnknown()

"""
    isdone(solutions::Solutions, pico_sat = solutions.pico_sat)

Return a `Bool`ean whether the `pico_sat` instance can yield any more solutions
without advancing the [`Solutions`](@ref PicoSAT.Solutions) iterator.

Finalize the iterator if it has not yet been finalized and is done.
"""
isdone(solutions::Solutions, pico_sat = solutions.pico_sat) = pico_sat == C_NULL ||
    let is_satisfiable = picosat_sat(pico_sat, -1) == 10
        is_satisfiable || finalize(solutions)
        !is_satisfiable
    end

"""
    iterate(solutions::Solutions, pico_sat = solutions.pico_sat)

If the status of `pico_sat` is satisfiable,
return a `Tuple` of the current solution and `pico_sat`.
Otherwise, `finalize` the `solutions` and return `nothing`.
"""
iterate(solutions::Solutions, pico_sat = solutions.pico_sat) = if !isdone(solutions, pico_sat)
    n = picosat_variables(pico_sat)
    assignments = Vector{Bool}(undef, n)

    clause = map(1:n) do atom
        literal = picosat_deref(pico_sat, atom)
        assignments[atom] = !signbit(literal)
        -atom * literal
    end

    add_clause(pico_sat, clause)
    assignments, pico_sat
end

"""
    print_dimacs(io, clauses, n)

Print the DIMACS format of the conjunctive `clauses`.

# Examples
```jldoctest
julia> PAndQ.PicoSAT.print_dimacs(stdout, [[-1, -2], [1, 2]], 2)
p cnf 2 2
-1 -2 0
1 2 0

julia> PAndQ.PicoSAT.print_dimacs(stdout, [[1, -2], [-1, 2]], 2)
p cnf 2 2
1 -2 0
-1 2 0
```
"""
function print_dimacs(io, clauses, n)
    _read, _write = pipe = Pipe()
    pico_sat = initialize(clauses, n)
    redirect_stdout(() -> picosat_print(pico_sat, FILE(RawFD(1), "w")), pipe)
    picosat_reset(pico_sat)
    close(_write)
    write(io, _read)
    nothing
end

end # PicoSAT
