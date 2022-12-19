
import Base: show

"""
    Show{A <: Any}
    Show(x::A, callback::Function)
    Show(x::A, text::String)

A wrapper to automatically enable the pretty-printing of ```x``` with the contents of ```text```.

By default, ```show(io::IO, ::MIME"text/plain", x::Show)```
is overloaded to enable both regular and pretty-printing, depending on the context.
This method calls the ```callback``` function with ```x``` as its argument.
The callback function returns a string to be shown.
Given ```text``` instead will automatically create the callback function ```_ -> text```.

To implement custom behavior and for more information, see Julia's documentation on [pretty-printing]
(https://docs.julialang.org/en/v1/manual/types/#man-custom-pretty-printing).

See also [`@Show`](@ref)

# Examples
```jldoctest
julia> r = p → (q → p)
Propositional:
  ¬(p ∧ q ∧ ¬p)

julia> s = Show(r, "p → (q → p)")
Show{Propositional}:
  p → (q → p)

julia> s()
Truth:
  ⊤
```
"""
struct Show{A <: Any} # or `Pretty`?
    x::A
    callback::Function
end

Show(x, text::String) = Show(x, _ -> text)

(x::Show)() = x.x()

"""
    @Show(expression)

Return an instance of [`Show`](@ref), whose ```text``` field is
set to ```string(expression)```.

# Examples
```jldocttest
julia> r = @Show p → (q → p)
Show{Propositional}:
  p → (q → p)

julia> r()
TruthValue:
  ⊤
```
"""
macro Show(expression)
    return :(Show($(esc(expression)), $(string(expression))))
end

show(io::IO, ::MIME"text/plain", x::Show) = print(io, typeof(x), ":\n  ", x.callback(x.x))

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
_print(p::Tuple{Not, Language}) = _print(p[1]) * "(" * _print(p[2]) * ")"
_print(p::Tuple{Not, Primitive}) = _print(p[1]) * _print(p[2])
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

show(io::IO, ::MIME"text/plain", p::Contingency) = print(io, nameof(typeof(p)), ":\n", _print(p))
show(io::IO, ::MIME"text/plain", p::Language) = print(io, nameof(typeof(p)), ":\n  ", _print(p))
show(io::IO, ::MIME"text/plain", p::Show{<:Language}) = print(io,
    nameof(typeof(p)), "{", nameof(typeof((p.x))), "}:\n  ",
    p.callback(p.x)
)
