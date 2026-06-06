
module Solvers

abstract type Solver end

for solver in ["PicoSAT", "Z3"]
    include("solvers/$solver.jl")
end

import Base: IteratorSize, eltype
using Base: SizeUnknown

IteratorSize(::Type{Solutions}) = SizeUnknown()

eltype(::Type{Solutions}) = Vector{Bool}

end # Solvers
