
using Combinatorics
using PrettyTables

(::Not)(::typeof(⊥)) = ⊤
(::Not)(::typeof(⊤)) = ⊥
(::Not)(p::Primitive) = Literal((Not(), p))
(::Not)(p::Literal{Primitive}) = ¬p.ϕ
(::Not)(p::Literal{Tuple{Not, Primitive}}) = last(p.ϕ)
(::Not)(p::Compound) = Propositional(Not(), p)
(::Not)(p::Propositional{<:Tuple{Not, Compound}}) = last(p.ϕ) # double negation elimination
function (::Not)(p::Normal{B}) where B <: Union{And, Or}
    clauses = map(clause -> map(¬, clause), p.clauses)
    b = B == And ? Or : And

    return Normal{b}(clauses)
end

(::And)(::typeof(⊤), ::typeof(⊤)) = ⊤
(::And)(::typeof(⊥), ::Truth) = ⊥ # domination law
(::And)(::typeof(⊥), ::Language) = ⊥
(::And)(::typeof(⊤), q::Truth) = q # identity law
(::And)(::typeof(⊤), q::Language) = q
(::And)(p::Language, q::Truth) = q ∧ p # commutative law
(::And)(p::Language, q::Language) = Propositional(And(), p, q)

(p::Union{Truth, Contingency})() = p
(p::Normal)() = Propositional(p)()

# ToDo: make type stable
function (p::Language)()
    primitives = get_primitives(p)
    n = length(primitives)
    truth_sets = multiset_permutations([⊤, ⊥], [n, n], n)
    valuations = map(truth_set -> map(Pair{Primitive, Truth}, primitives, truth_set), truth_sets)
    truths = map(valuation -> interpret(p -> Dict(valuation)[p], p), valuations)
    
    union(truths) == [⊤] && return ⊤
    union(truths) == [⊥] && return ⊥
    return Contingency(map(Pair, valuations, truths))
end

"""
    interpret(valuation, p::Language)

Given a valuation function that maps from the [`Primitive`](@ref)
propositions in ```p``` to their respective [`Truth`](@ref) values,
assign a truth value to ```p```.

See also [`Language`](@ref).
"""
interpret(valuation, p::Language) = p(Dict(map(p -> p => valuation(p), get_primitives(p))))

(p::Primitive)(interpretations) = interpretations[p]
(p::Literal{Primitive})(interpretations) = p.ϕ(interpretations)
(p::Literal{Tuple{Not, Primitive}})(interpretations) = first(p.ϕ)(last(p.ϕ)(interpretations))
(p::Propositional)(interpretations) = first(p.ϕ)(map(ϕ -> ϕ(interpretations), Base.tail(p.ϕ))...)
(p::Normal)(interpretations) = Propositional(p)(interpretations)

"""
    p == q
    ==(p::Language, q::Language)
    isequal(p::Language, q::Language)

Returns a boolean indicating whether ```p``` and ```q``` are logically equivalent.

See also [`Language`](@ref).

!!! info
    The ```≡``` symbol is sometimes used to represent logical equivalence.
    However, Julia uses ```≡``` as an alias for the builtin function ```===```
    which cannot have methods added to it.
    Use this function to compare identity rather than equivalence.

# Examples
```
julia> p == ¬p
false

julia> (p → q) ∧ (p ← q) == ¬(p ⊻ q)
true

julia> (p → q) ∧ (p ← q) === ¬(p ⊻ q)
false
```
"""
Base.:(==)(p::TP, q::TP) where TP <: Union{Truth, Primitive} = p === q
Base.:(==)(p::Language, q::Language) = is_tautology(p ↔ q)

"""
    is_tautology(p::Language)

Returns a boolean on whether the given proposition is a [`tautology`](@ref).

This function is equivalent to ```p == ⊤```.

See also [`Language`](@ref) and [`==`](@ref).

# Examples
```jldoctest
julia> is_tautology(⊤)
true

julia> is_tautology(p)
false

julia> is_tautology(¬(p ∧ ¬p))
true
```
"""
is_tautology(p::Language) = _is_tautology(p())

_is_tautology(p::typeof(⊤)) = true
_is_tautology(p) = false

"""
    is_contradiction(p::Language)

Returns a boolean on whether the given proposition is a [`contradiction`](@ref).

This function is equivalent to ```p == ⊥```.

See also [`Language`](@ref) and [`==`](@ref).

# Examples
```jldoctest
julia> is_contradiction(⊥)
true

julia> is_contradiction(p)
false

julia> is_contradiction(p ∧ ¬p)
true
```
"""
is_contradiction(p::Language) = p == ⊥

"""
    is_truth(p::Language)

Returns a boolean on whether the given proposition is a [`Truth`](@ref)
(either a [`tautology`](@ref) or [`contradiction`](@ref)).

See also [`Language`](@ref).

# Examples
```jldoctest
julia> is_truth(⊤)
true

julia> is_truth(p ∧ ¬p)
true

julia> is_truth(p)
false

julia> is_truth(p ∧ q)
false
```
"""
is_truth(p::Language) = _is_truth(p())

_is_truth(p::Truth) = true
_is_truth(p) = false

"""
    is_contingency(p::Language)

Returns a boolean on whether the given proposition is a contingency
(neither a [`tautology`](@ref) or [`contradiction`](@ref)).

See also [`Language`](@ref).

# Examples
```jldoctest
julia> is_contingency(⊤)
false

julia> is_contingency(p ∧ ¬p)
false

julia> is_contingency(p)
true

julia> is_contingency(p ∧ q)
true
```
"""
is_contingency(p::Language) = !is_truth(p)

"""
    is_satisfiable(p::Language)

Returns a boolean on whether the given proposition is satisfiable (not a [`contradiction`](@ref)).

See also [`Language`](@ref).

# Examples
```jldoctest
julia> is_satisfiable(⊤)
true

julia> is_satisfiable(p ∧ ¬p)
false

julia> is_satisfiable(p)
true

julia> is_satisfiable(p ∧ q)
true
```
"""
is_satisfiable(p::Language) = !is_contradiction(p)

"""
    is_falsifiable(p::Language)

Returns a boolean on whether the given proposition is falsifiable (not a [`is_tautology`](@ref)).

See also [`Language`](@ref).

# Examples
```jldoctest
julia> is_falsifiable(⊥)
true

julia> is_falsifiable(¬(p ∧ ¬p))
false

julia> is_falsifiable(p)
true

julia> is_falsifiable(p ∧ q)
true
```
"""
is_falsifiable(p::Language) = !is_tautology(p)

"""
    @truth_table p
    @truth_table(ps...)

Print a truth table for the given propositions.

The first row of the header is the expression representing that column's proposition,
the second row indicates that expression's type,
and the third row identifies the statements for [`Primitive`](@ref) propositions.

!!! info
    If a variable contains a primitive, there is no expression to label that primitive.
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
function truth_table(trees::Vector{<:Language}, trees_str, leaves, leaves_str)
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

    pretty_interpretations = map(_print, interpretations)

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
    append!(_header, map(_print, _truths))

    sub_header = map(nameof ∘ typeof, vcat(primitives, _sub_header, _truths))
    sub_sub_header = vcat(
        map(primitive -> "\"" * primitive.statement * "\"", primitives),
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
