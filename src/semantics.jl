
using PrettyTables

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
is_tautology(p) = union(map(last, p())) == [⊤]

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
    is_contingency(p::Language)

Returns a boolean on whether the given proposition is a contingency
(neither a [`tautology`](@ref) or [`contradiction`](@ref)).

While this function is equivalent to ```p != ⊤ && p != ⊥```, ```is_contingency(p)``` is preferred
because the former expression will give an incorrect result if ```p``` is not a subtype of ```Language```.

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
is_contingency(p::Language) = !is_tautology(p) && !is_contradiction(p)

"""
    is_satisfiable(p::Language)

Returns a boolean on whether the given proposition is satisfiable (not a [`contradiction`](@ref)).

While this function is equivalent to ```p != ⊥```, ```is_satisfiable(p)``` is preferred
because the former expression will give an incorrect result if ```p``` is not a subtype of ```Language```.

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

While this function is equivalent to ```p != ⊤```, ```is_falsifiable(p)``` is preferred
because the former expression will give an incorrect result if ```p``` is not a subtype of ```Language```.

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
    p == q
    ==(p::Language, q::Language)
    isequal(p::Language, q::Language)

Returns a boolean indicating whether ```p``` and ```q``` are logically equivalent.

See also [`Language`](@ref).

!!! info
    The ```≡``` symbol is sometimes used to represent logical equivalence.
    However, Julia uses ```≡``` as an alias for the builtin function ```===```
    which cannot have methods added to it.

# Examples
```
julia> p == ¬p
false

julia> julia> p ∨ q == ¬(¬q ∧ ¬p)
true

julia> isequal((p → q) ∧ (p ← q), ¬(p ⊻ q))
true
```
"""
Base.:(==)(p::Language, q::Language) = is_tautology(p ↔ q)
Base.:(==)(p::Primitive, q::Primitive) = p === q
Base.:(==)(p::Truth, q::Truth) = p === q

(p::Truth)(interpretations) = p
(p::Primitive)(interpretations) = interpretations[p]
(p::Propositional{<:Primitive})(interpretations) = p.ϕ(interpretations)
(p::Propositional{<:Tuple})(interpretations) = first(p.ϕ)(map(ϕ -> ϕ(interpretations), Base.tail(p.ϕ))...)
"""
    interpret(valuation, ϕ::Language)

Given a valuation function that maps from the [`Primitive`](@ref)
propositions in ```ϕ``` to their respective [`Truth`](@ref) values,
assign a truth value to ```ϕ```.

See also [`Language`](@ref).

```jldoctest
julia> mapping = Dict(p => ⊥, q => ⊤);

julia> valuation = r -> mapping[r];

julia> PAQ.interpret(valuation, p ∧ q)
⊥

julia> PAQ.interpret(valuation, p → q)
⊤
```
"""
interpret(valuation, ϕ::Language) = ϕ(Dict(map(p -> p => valuation(p), get_primitives(ϕ))))

# TODO: simplify logic
# TODO: fix subheader of `@truth_table ⊥ p ∧ ¬p`
# TODO: fix `@truth_table p ∧ ¬(p ∧ ¬p)`
# TODO: write docstring
function truth_table(trees, trees_str, leaves, leaves_str)
    #=
    Set interface?
        Base.length(p::Language) = 1
        Base.iterate(p::Language) = p
    =#

    eltype(trees) <: Language || throw(ErrorException("Every expression must be a subtype of `Language`"))

    primitives = get_primitives(trees...)
    n = length(primitives)
    truth_sets = multiset_permutations([⊤, ⊥], [n, n], n)
    valuations = map(truth_set -> zip(primitives, truth_set), truth_sets)

    merge_string = (x, y) -> x == y || y == "" ? x : x * ", " * y

    _sub_header = Language[]
    labels = String[]
    assignments = Vector{Truth}[]
    for (tree, tree_str) in zip(trees, trees_str)
        if tree isa Primitive
            continue
        end

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

    valuation_matrix = mapreduce(permutedims, vcat, truth_sets)
    assignment_matrix = reduce(hcat, assignments, init = Matrix(undef, 2^n, 0))
    interpretations = hcat(valuation_matrix, assignment_matrix)

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

    sub_header = map(nameof ∘ typeof, vcat(primitives, _sub_header))
    sub_sub_header = vcat(map(primitive -> "\"" * primitive.statement * "\"", primitives), map(_ -> "", _sub_header))
    header = (_header, sub_header, sub_sub_header)

    pretty_table(
        interpretations,
        header = header,
        alignment = :l,
        body_hlines = collect(0:2:2^n),
        crop = :none
    )
end
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
julia> @truth_table p∧q p→q
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
