
module PicoSAT

using libpicosat_jll

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
    picosat_add(pico_sat, literal)

Append the `literal` to the `pico_sat` instance's current disjunctive clause.
"""
picosat_add(pico_sat, literal) =
    @ccall libpicosat.picosat_add(pico_sat::Ptr{Cvoid}, literal::Cint)::Cint

"""
    picosat_deref(pico_sat, atom)

Return the assignment of the `atom`, where `1`, `-1`, and `0`
indicate `true`, `false`, and unknown, respectively.

This function must be called after verifying that the status of [`picosat_sat`](@ref) is satisfiable.
"""
picosat_deref(pico_sat, atom) =
    @ccall libpicosat.picosat_deref(pico_sat::Ptr{Cvoid}, atom::Cint)::Cint

"""
    picosat_init()

Construct a new PicoSAT instance and return a pointer to it.
"""
picosat_init() = @ccall libpicosat.picosat_init()::Ptr{Cvoid}

"""
    picosat_print(pico_sat, file)

Write the DIMACS format of the `pico_sat` instance's proposition to the given `file`.
"""
picosat_print(pico_sat, file) =
    @ccall libpicosat.picosat_print(pico_sat::Ptr{Cvoid}, file::Ptr{Cvoid})::Cvoid

"""
    picosat_reset(pico_sat)

Destruct the `pico_sat` instance.
"""
picosat_reset(pico_sat) = @ccall libpicosat.picosat_reset(pico_sat::Ptr{Cvoid})::Cvoid

"""
    picosat_sat(pico_sat, limit)

Search for a satisfiable assignment of the `pico_sat` instance's proposition and return
`0`, `10`, or `20` if the status is unknown, satisfiable, or unsatisfiable, respectively.
"""
picosat_sat(pico_sat, limit) =
    @ccall libpicosat.picosat_sat(pico_sat::Ptr{Cvoid}, limit::Cint)::Cint

"""
    picosat_variables(pico_sat)

Return the number of unique atoms in the `pico_sat` instance.
"""
picosat_variables(pico_sat) =
    @ccall libpicosat.picosat_variables(pico_sat::Ptr{Cvoid})::Cint

end # module
