
import Base: show

"""
    Pretty{L <: Language}
    Pretty(p::L, text::String)

A wrapper to automatically enable the pretty-printing of ```p``` with the contents of ```text```.

By default, ```show(io::IO, ::MIME"text/plain", x::Pretty)```
is overloaded to enable both regular and pretty-printing, depending on the context.
This method pretty-prints ```p``` with the contents of ```text```.

See also [`@Pretty`](@ref)

# Examples
```jldoctest
julia> r = p → (q → p)
Propositional:
  ¬(p ∧ q ∧ ¬p)

julia> s = Pretty(r, "p → (q → p)")
Pretty{Propositional}:
  p → (q → p)

julia> s()
Truth:
  ⊤
```
"""
struct Pretty{L <: Language}
    p::L
    text::String
end

(p::Pretty)() = p.p()
(::Not)(p::Pretty) = ¬p.p

"""
    @Pretty(expression)

Return an instance of [`Pretty`](@ref), whose ```text``` field is
set to ```string(expression)```.

# Examples
```jldocttest
julia> r = @Pretty p → (q → p)
Pretty{Propositional}:
  p → (q → p)

julia> r()
TruthValue:
  ⊤
```
"""
macro Pretty(expression)
    return :(Pretty($(esc(expression)), $(string(expression))))
end

# ToDo: clean-up
i(interpretation, interpretations) = interpretation == last(interpretations) ? "" : "\n"
h(literal, interpretation) = literal == last(interpretation) ? "" : ", "
g(literal) = _print(first(literal)) * " => " * _print(last(literal))
f(interpretation) = "  [" * mapreduce(literal -> g(literal) * h(literal, first(interpretation)), *, first(interpretation)) * "] => " * _print(last(interpretation))

_print(::Not) = "¬"
_print(::And) = " ∧ "
_print(::Or) = " ∨ "
_print(::typeof(⊤)) = "⊤"
_print(::typeof(⊥)) = "⊥"
_print(p::Primitive) = p.statement
_print(p::Contingency) = mapreduce(interpretation -> f(interpretation) * i(interpretation, p.interpretations), *, p.interpretations)
_print(p::Literal) = _print(p.ϕ)
_print(p::Propositional) = _print(p.ϕ)
_print(p::Tuple{Not, Primitive}) = _print(p[1]) * _print(p[2])
_print(p::Tuple{Not, Language}) = _print(p[1]) * "(" * _print(p[2]) * ")"
_print(p::Tuple{And, Compound, Compound}) = _print(p[2]) * _print(p[1]) * _print(p[3])
function _print(p::Normal{B}) where B <: Union{And, Or}
    b = first(setdiff!(Set([And, Or]), [B]))
    s = ""

    for clause in p.clauses
        s *= "("

        for literal in clause
            s *= _print(literal)

            if literal !== last(clause)
                s *= _print(b())
            end
        end

        s *= ")"

        if clause !== last(p.clauses)
            s *= _print(B())
        end
    end

    return s
end

show(io::IO, p::Language) = print(io, _print(p))
show(io::IO, ::MIME"text/plain", p::Language) = print(io, nameof(typeof(p)), ":\n  ", p)
show(io::IO, ::MIME"text/plain", p::Contingency) = print(io, nameof(typeof(p)), ":\n", p)
show(io::IO, ::MIME"text/plain", p::Pretty) = print(io,
    nameof(typeof(p)), "{", nameof(typeof((p.p))), "}:\n  ",
    p.text
)
