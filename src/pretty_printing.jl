
import Base: repr, show

"""
    repr(p::Language)
    repr(::MIME"text/plain", p::Language)

Return a string representation of the given proposition.

!!! tip
    Use the [`Pretty`](@ref) wrapper or [`@pretty`](@ref) macro to get a formatted string.
    Use `MIME("text/plain")` to get the pretty-printed representation.

```jldoctest
julia> p ↔ q
Propositional:
  ¬("p" ∧ ¬"q") ∧ ¬(¬"p" ∧ "q")

julia> repr(p ↔ q)
"¬(\\"p\\" ∧ ¬\\"q\\") ∧ ¬(¬\\"p\\" ∧ \\"q\\")"

julia> repr(Pretty(p ↔ q))
"¬(p ∧ ¬q) ∧ ¬(¬p ∧ q)"

julia> repr(@pretty p ↔ q)
"p ↔ q"

julia> repr(MIME("text/plain"), @pretty p ↔ q)
"Pretty{Propositional}:\\n  p ↔ q"
```
"""
repr(::typeof(⊤)) = "⊤"
repr(::typeof(⊥)) = "⊥"
repr(p::Primitive) = "\"" * p.statement * "\""
repr(p::Contingency) = mapreduce(interpretation -> f(interpretation) * i(interpretation, p.interpretations), *, p.interpretations)
repr(p::Literal) = repr(p.ϕ)
repr(p::Propositional) = repr(p.ϕ)
repr(p::Tuple{Not, Primitive}) = repr(p[1]) * repr(p[2])
repr(p::Tuple{Not, Language}) = repr(p[1]) * "(" * repr(p[2]) * ")"
repr(p::Tuple{And, Compound, Compound}) = repr(p[2]) * " " * repr(p[1]) * " " * repr(p[3])
function repr(p::Normal{B}) where B <: Union{And, Or}
    b = first(setdiff!(Set([And, Or]), [B]))
    s = ""

    for clause in p.clauses
        s *= "("

        for literal in clause
            s *= repr(literal)

            if literal !== last(clause)
                s *= " " * repr(b()) * " "
            end
        end

        s *= ")"

        if clause !== last(p.clauses)
            s *= " " * repr(B()) * " "
        end
    end

    return s
end

repr(::Not) = "¬"
repr(::And) = "∧"
repr(::Or) = "∨"

# ToDo: clean-up
i(interpretation, interpretations) = interpretation == last(interpretations) ? "" : "\n"
h(literal, interpretation) = literal == last(interpretation) ? "" : ", "
g(literal) = repr(first(literal)) * " => " * repr(last(literal))
f(interpretation) = "  [" * mapreduce(literal -> g(literal) * h(literal, first(interpretation)), *, first(interpretation)) * "] => " * repr(last(interpretation))

show(io::IO, p::Language) = print(io, repr(p))
function show(io::IO, ::MIME"text/plain", p::Language)
    indent = p isa Contingency ? "" : "  "
    print(io, nameof(typeof(p)), ":\n", indent, p)
end

# TODO: make composable
"""
    Pretty{L <: Language} <: Compound
    Pretty(p::L[, text::String])

A wrapper to automatically enable the pretty-printing of ```p``` with the contents of ```text```.

The default value of ```text``` will pretty-print ```p``` the same as its regular pretty-printing,
except without quotation marks.

See also [`Compound`](@ref) and [`@pretty`](@ref).

# Examples
```jldoctest
julia> r = p → (q → p)
Propositional:
  ¬("p" ∧ "q" ∧ ¬"p")

julia> Pretty(r)
Pretty{Propositional}:
  ¬(p ∧ q ∧ ¬p)

julia> Pretty(r, "p → (q → p)")
Pretty{Propositional}:
  p → (q → p)
```
"""
struct Pretty{L <: Language} <: Compound
    p::L
    text::String

    Pretty(p::L, text::String = replace(repr(p), "\"" => "")) where L <: Language = new{L}(p, text)
end

(p::Pretty)() = p.p()
(::Not)(p::Pretty) = ¬p.p

show(io::IO, p::Pretty) = print(io, p.text)
function show(io::IO, ::MIME"text/plain", p::Pretty)
    indent = p isa Contingency ? "" : "  "
    print(io, nameof(typeof(p)), "{", nameof(typeof((p.p))), "}:\n", indent, p.text)
end
"""
    @Pretty(expression)

Return an instance of [`Pretty`](@ref), whose ```text``` field is
set to ```string(expression)```.

# Examples
```jldocttest
julia> p → (q → p)
Propositional:
  ¬("p" ∧ "q" ∧ ¬"p")

julia> @pretty p → (q → p)
Pretty{Propositional}:
  p → (q → p)
```
"""
macro pretty(expression)
    return :(Pretty($(esc(expression)), $(string(expression))))
end
