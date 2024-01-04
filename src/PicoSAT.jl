
module PicoSAT

import Base: IteratorSize, eltype, isdone, iterate
using Base: Fix2, Generator, HasEltype, SizeUnknown, Splat
using Base.Iterators: Enumerate, Filter
using libpicosat_jll
using ..PAndQ: PAndQ
using ..PAndQ

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

"""
    add_clause(pico_sat, clause)

Modify the `pico_sat` instance's proposition to be the
conjunction of itself and the disjunctive `clause`.
"""
function add_clause(pico_sat, clause)
    for literal in clause
        picosat_add(pico_sat, literal)
    end
    picosat_add(pico_sat, 0)
end

"""
    initialize(clauses)

Return a PicoSAT pointer with its proposition being a conjunction of the disjunctive `clauses`.
"""
function initialize(clauses)
    pico_sat = picosat_init()
    for clause in clauses
        add_clause(pico_sat, clause)
    end
    pico_sat
end

"""
    finalize!(solutions)

If the argument has not been finalized, call [`picosat_reset`](@ref)
on its PicoSAT pointer and then set the pointer equal to `C_NULL`.
"""
function finalize!(solutions)
    pico_sat = solutions.pico_sat
    if pico_sat != C_NULL
        solutions.pico_sat = C_NULL
        picosat_reset(pico_sat)
    end
end

"""
    Solutions
    Solutions(clauses)

A stateful iterator of [`valuations`](@ref) that satisfy the given proposition.
"""
mutable struct Solutions
    pico_sat::Ptr{Cvoid}

    Solutions(clauses) = finalizer(finalize!, new(initialize(clauses)))
end

"""
    eltype(::Type{Solutions})

# Examples
```jldoctest
julia> eltype(PAndQ.PicoSAT.Solutions)
Base.Generator{Base.Iterators.Enumerate{Base.Iterators.Filter{Base.Fix2{typeof(!=), Int64}, Vector{Int32}}}, Base.Splat{typeof(*)}}
```
"""
eltype(::Type{Solutions}) = Generator{Enumerate{Filter{Fix2{typeof(!=), Int64}, Vector{Int32}}}, Splat{typeof(*)}}

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
    is_satisfiable(pico_sat)
"""
is_satisfiable(pico_sat) = picosat_sat(pico_sat, -1) == 10

"""
    isdone(solutions::Solutions, pico_sat = solutions.pico_sat)
"""
isdone(solutions::Solutions, pico_sat = solutions.pico_sat) = pico_sat == C_NULL || !is_satisfiable(pico_sat)

"""
    iterate(solutions::Solutions, pico_sat = solutions.pico_sat)

If the status of `pico_sat` [`is_satisfiable](@ref PAndQ.PicoSAT.is_satisfiable),
return a `Tuple` of the current solution and `pico_sat`.
Otherwise, [`finalize!`](@ref) the `solutions` and return `nothing`.
"""
iterate(solutions::Solutions, pico_sat = solutions.pico_sat) =
    if !isdone(solutions)
        if is_satisfiable(pico_sat)
            atoms_truths = enumerate(Iterators.filter(!=(0), map(
                atom -> picosat_deref(pico_sat, atom), 1:picosat_variables(pico_sat))))
            add_clause(pico_sat, Iterators.map((-) ∘ splat(flipsign), atoms_truths))
            Iterators.map(splat(*), atoms_truths), pico_sat
        else finalize(solutions)
        end
    end

"""
    dimacs(io, clauses)

# Examples
```jldoctest
julia> PAndQ.PicoSAT.dimacs(stdout, ((-1, -2), (1, 2)))
p cnf 2 2
-1 -2 0
1 2 0

julia> PAndQ.PicoSAT.dimacs(String, ((1, -2), (-1, 2)))
"p cnf 2 2\\n1 -2 0\\n-1 2 0\\n"
```
"""
function dimacs(io::IO, clauses)
    _read, _write = pipe = Pipe()
    pico_sat = initialize(clauses)
    (file = @ccall fdopen(1::Cint, "w"::Cstring)::Ptr{Cvoid}) == C_NULL && error("could not open file")
    redirect_stdout(pipe) do
        picosat_print(pico_sat, file)
        @ccall(fclose(file::Ptr{Cvoid})::Cint) == 0 || error("could not close file")
    end
    picosat_reset(pico_sat)
    close(_write)
    write(io, _read)
    nothing
end
function dimacs(::Type{String}, clauses)
    buffer = IOBuffer()
    dimacs(buffer, clauses)
    String(take!(buffer))
end
dimacs(path::String, clauses) = open(file -> dimacs(file, clauses), path; truncate = true)

end # module
