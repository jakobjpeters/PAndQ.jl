
module InteractiveUtilsExtension

import PAndQ: generate
using InteractiveUtils: subtypes
using PAndQ: Proposition, Compound, Expressive

"""
    union_all_subtypes(t)

Return a vector of every `UnionAll` subtype of `t`.
```
"""
union_all_subtypes(t::UnionAll) = [t]
union_all_subtypes(t::DataType) = mapreduce(union_all_subtypes, vcat, subtypes(t))

"""
    generate(::Type{T} = Proposition) where T <: DataType

Generate a [`Proposition`](@ref) that is a subtype of `T where T` is any of
[`Proposition`](@ref), [`Compound`](@ref), or [`Expressive`](@ref).
"""
generate() = generate(Proposition)
for T in (Proposition, Compound, Expressive)
    @eval generate(::Type{$T}) = generate(rand(union_all_subtypes($T)))
end

end # module
