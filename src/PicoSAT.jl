
module PicoSAT

using libpicosat_jll

picosat_init() = @ccall libpicosat.picosat_init()::Ptr{Cvoid}

picosat_reset(pico_sat) = @ccall libpicosat.picosat_reset(pico_sat::Ptr{Cvoid})::Cvoid

picosat_add(pico_sat, literal) =
    @ccall libpicosat.picosat_add(pico_sat::Ptr{Cvoid}, literal::Cint)::Cint

picosat_sat(pico_sat, limit) =
    @ccall libpicosat.picosat_sat(pico_sat::Ptr{Cvoid}, limit::Cint)::Cint

picosat_variables(pico_sat) =
    @ccall libpicosat.picosat_variables(pico_sat::Ptr{Cvoid})::Cint

picosat_deref(pico_sat, atom) =
    @ccall libpicosat.picosat_deref(pico_sat::Ptr{Cvoid}, atom::Cint)::Cint

picosat_print(pico_sat, file) =
    @ccall libpicosat.picosat_print(pico_sat::Ptr{Cvoid}, file::Ptr{Cvoid})::Cvoid

"""
    add_clause(pico_sat, clause)
"""
function add_clause(pico_sat, clause)
    for literal in clause
        picosat_add(pico_sat, literal)
    end
    picosat_add(pico_sat, 0)
end

"""
    initialize(p)
"""
function initialize(p)
    pico_sat = picosat_init()
    for clause in p.clauses
        add_clause(pico_sat, clause)
    end
    pico_sat
end

end # module
