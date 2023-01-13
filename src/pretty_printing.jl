
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

"""
    @truth_table p
    @truth_table(ps...)

Print a truth table for the given propositions.

The first row of the header is the expression representing that column's proposition,
the second row indicates that expression's type,
and the third row identifies the statements for [`Primitive`](@ref) propositions.

!!! info
    If a variable is a [`Compound`](@ref), there is no expression to label that primitive.
    As such, the first row in the header will be blank.
    However, the identifying statement is still known and will be displayed in the third row.
    Use [`get_primitives`](@ref) to resolve this uncertainty.

Logically equivalent propositions will be placed in the same column
with their expressions in the header seperated by a comma.

In this context, [`⊤`](@ref tautology) and [`⊥`](@ref contradiction) can be interpreted as *true* and *false*, respectively.

See also [`Language`](@ref).

# Examples
```jldoctest
julia> @truth_table p ∧ q p → q
┌───────────┬───────────┬───────────────┬───────────────┐
│ p         │ q         │ p ∧ q         │ p → q         │
│ Primitive │ Primitive │ Propositional │ Propositional │
│ "p"       │ "q"       │               │               │
├───────────┼───────────┼───────────────┼───────────────┤
│ ⊤         │ ⊤         │ ⊤             │ ⊤             │
│ ⊤         │ ⊥         │ ⊥             │ ⊥             │
├───────────┼───────────┼───────────────┼───────────────┤
│ ⊥         │ ⊤         │ ⊥             │ ⊤             │
│ ⊥         │ ⊥         │ ⊥             │ ⊤             │
└───────────┴───────────┴───────────────┴───────────────┘
```
"""
macro truth_table(expressions...)
    f = expression -> typeof(expression) <: Union{Symbol, String} ? [expression] : mapreduce(f, vcat, expression.args[2:end])
    propositions = reduce(union, map(f, expressions))

    return :(
        truth_table(
            [$(map(esc, expressions)...)],
            map(string, $expressions),
            [$(map(esc, propositions)...)],
            map(string, $propositions)
        )
    )
end

# ToDo: holy guacamole this function is a mess
# ToDo: simplify logic
# ToDo: fix subheader combining types
# ToDo: fix `@truth_table p ∧ ¬(p ∧ ¬p)`
# ToDo: write docstring - define behavior
# ToDo: write tests
function truth_table(_trees::Vector{<:Language}, trees_str, leaves, leaves_str)
    trees = map(tree -> tree isa Pretty ? tree.p : tree,_trees)

    primitives = get_primitives(trees...)
    n = length(primitives)
    truth_sets = multiset_permutations([⊤, ⊥], [n, n], n)
    valuations = map(truth_set -> zip(primitives, truth_set), truth_sets)

    merge_string = (x, y) -> x == y || y == "" ? x : x * ", " * y

    _sub_header = []
    labels = String[]
    assignments = Vector{Truth}[]
    for (tree, tree_str) in filter(pair -> !isa(first(pair), Truth), map(Pair, trees, trees_str))
        tree isa Primitive && continue

        truths = map(valuation -> interpret(p -> Dict(valuation)[p], tree), valuations)

        if truths in assignments
            i = findfirst(assignment -> assignment == truths, assignments)
            labels[i] = merge_string(labels[i], tree_str)
        else
            push!(_sub_header, tree)
            push!(labels, tree_str)
            push!(assignments, truths)
        end
    end

    _truths = filter(tree -> tree isa Truth, trees)
    temp = hcat(map(p -> repeat([p], 2^n), _truths)...)
    valuation_matrix = mapreduce(permutedims, vcat, truth_sets)
    assignment_matrix = reduce(hcat, assignments, init = Matrix(undef, 2^n, 0))
    interpretations = reduce(hcat, [valuation_matrix, assignment_matrix])

    if !isempty(temp)
        interpretations = reduce(hcat, [valuation_matrix, assignment_matrix, temp])
    end

    pretty_interpretations = map(repr, interpretations)

    make_header = (ps, ps_str) -> begin
        ___header = Dict{Primitive, Vector{String}}()

        for (p, p_str) in zip(ps, ps_str)
            if p isa Primitive
                if p in keys(___header)
                    push!(___header[p], p_str)
                else
                    ___header[p] = [p_str]
                end
            end
        end

        return ___header
    end

    header_domains = [
        (leaves, leaves_str),
        (trees, trees_str),
        (primitives, map(primitive -> "", primitives))
    ]
    headers = map(header_domain -> make_header(header_domain...), header_domains)
    __header = mergewith!(union ∘ vcat, headers...)
    _header = map(primitive -> reduce(merge_string, __header[primitive]), primitives)
    push!(_header, labels...)
    append!(_header, map(repr, _truths))

    sub_header = map(nameof ∘ typeof, vcat(primitives, _sub_header, _truths))
    sub_sub_header = vcat(
        map(repr, primitives),
        map(_ -> "", vcat(_sub_header, _truths)),
    )
    header = (_header, sub_header, sub_sub_header)

    pretty_table(
        pretty_interpretations,
        header = header,
        alignment = :l,
        body_hlines = collect(0:2:2^n),
        crop = :none
    )
end
