
import Base: ==, copy, hash, show
using AbstractTrees: AbstractTrees
using Base: isexpr, FlatteningRF
using .Iterators: Stateful, countfrom
using Bijections: Bijection
using Moshi: Data.@data, Derive.@derive, Match.@match
using ReplMaker: complete_julia, initrepl

# use `WeakKeyDict` or `WeakRef` as proposition cache?

export
    PicoSAT, Z3,
    ⋀, conjunction, ⋁, disjunction,
    atomize, @atomize, @variables,
    install_atomize_mode,
    value,
    @match,
    Box, unbox,
    constant, generated, variable

macro atomize(x)
    esc(:($(atomize(x))))
end

macro variables(ps::Symbol...) esc(quote
    $(map(p -> :($p = $(variable(p))), ps)...)
    $Expression.Type{Union{}}[$(ps...)]
end) end

atomize(x::Symbol) = :((@isdefined $x) ? $x : $(variable(x)))
function atomize(x::Expr)
    head, args = x.head, x.args
    if length(x.args) == 0 || (head == :macrocall && first(args) == Symbol("@atomize")) x
    elseif head == :$; :($constant($(only(x.args))))
    elseif head == :kw Expr(head, args[1], atomize(args[2]))
    elseif head in [:struct, :where] x # TODO
    else # TODO
        y = Expr(head)
        y_args = y.args
        b = head in [:<:, :(=), :->, :function]
        b && push!(y_args, args[1])
        for arg in args[1 + b:end]
            push!(y_args, atomize(arg))
        end
        y
    end
end
atomize(x) = x

function install_atomize_mode(; start_key = "\\M-a", prompt_text = "atomize> ", prompt_color = :cyan, kwargs...)
    initrepl(atomize ∘ Meta.parse;
        mode_name = :atomize,
        valid_input_checker = complete_julia,
        startup_text = false,
        prompt_text,
        start_key,
        prompt_color,
        kwargs...
    )
    @info "The `atomize` REPL mode has been installed: press [$start_key] to enter and [Backspace] to exit"
end

const ⋀ = conjunction(ps) = fold((∧) => ps)
const ⋁ = disjunction(ps) = fold((∨) => ps)

valuations(n) = Iterators.map(i -> map((!) ∘ Bool, digits(i; base = 2, pad = n)), 0:(2 ^ big(n) - 1))

const counter = Stateful(countfrom(1))

struct Box
    value
end

==(b::Box, _b::Box) = b.value == _b.value

copy(b::Box) = Box(copy(b.value))

hash(b::Box, h::UInt) = hash((Box, b.value), h)

unbox(t::Type, b::Box) = b.value::t
unbox(b::Box) = unbox(Any, b)

@data Either{L, R} begin
    Left(L)
    Right(R)
end

@derive Either[Eq, Hash, Show]

copy(e::Either.Type{L, R}) where {L, R} = @match e begin
    Either.Left(x) => Either.Left{L, R}(copy(x))
    Either.Right(x) => Either.Right{L, R}(copy(x))
end

@data Maybe{T} begin
    Nothing
    Some(T)
end

@derive Maybe[Eq, Hash, Show]

copy(m::Maybe.Type) = @match m begin
    Maybe.Some(x) => Maybe.Some(copy(x))
    _ => m
end

@data Atom{T} begin
    Constant(T)
    Generated(Int)
    Variable(Symbol)
end

@derive Atom[Eq, Hash]

copy(a::Atom.Type) = @match a begin
    Atom.Constant(x) => Atom.Constant(copy(x))
    _ => a
end

show(io::IO, ::MIME"text/plain", a::Atom.Type) = @match a begin
    Atom.Constant(x) => begin
        print(io, "\$(")
        show(io, x)
        print(io, ")")
    end
    Atom. Generated(x) || Atom.Variable(x) => print(io, x)
end

const Clauses = Vector{Vector{Int}}

@data Normal begin
    Conjunction(Clauses)
    Disjunction(Clauses)
    Negation(Vector{Pair{Symbol, Vector{Either.Type{Int, Int}}}})
end

@derive Normal[Eq, Hash, Show]

copy(c::Normal.Type) = @match c begin
    Normal.Conjunction(x) => Normal.Conjunction(copy(x))
    Normal.Disjunction(x) => Normal.Disjunction(copy(x))
    Normal.Negation(x) => Normal.Negation(copy(x))
end

@data Expression{T} begin
    Atom(Atom.Type{T}, UInt)
    Error(Vector{String})
    Nested(Symbol, Vector{Either.Type{Expression{T}, Expression{Union{}}}}, UInt)
    Normal(Normal.Type, Bijection{Int, Either.Type{Atom.Type{T}, Atom.Type{Union{}}}}, UInt)
    Operator(Symbol)
end

==(e::Expression.Type, _e::Expression.Type) = @match (e, _e) begin
    (Expression.Atom(x, h), Expression.Atom(_x, _h)) => h == _h && x == _x
    (Expression.Error(x), Expression.Error(_x)) => x == _x
    (Expression.Nested(x, y, h), Expression.Nested(_x, _y, _h)) => h == _h && flatten(x, y) == flatten(_x, _y)
    (Expression.Normal(x, y, h), Expression.Normal(_x, _y, _h)) => h == _h && x == _x && y == _y
    _ => false
end

copy(e::Expression.Type) = @match e begin
    Expression.Atom(x, _) => atom(copy(x))
    Expression.Error(x) => Expression.Error(copy(x))
    Expression.Nested(x, y, _) => nested(x, copy(y))
    Expression.Normal(x, y, _) => normal(copy(x), copy(y))
end

hash(e::Expression.Type, h) = @match e begin
    Expression.Atom(_, _h) => hash(_h, h)
    Expression.Error(x) => hash((Expression.Error, x), h)
    Expression.Nested(_, _, _h) => hash((Expression.Nested, _h), h)
    Expression.Normal(_, _, _h) => hash(_h, h)
end

show(io::IO, m::MIME"text/plain", e::Expression.Type) = @match e begin
    Expression.Atom(x, _) => show(io, m, x)
    _ => show(io, e)
end

atom(x) = Expression.Atom(x, hash((Expression.Atom, x)))
nested(x, y) = Expression.Nested(x, y, hash((x, y)))
normal(x, y) = Expression.Normal(x, y, hash((Expression.Normal, x, y)))

constant(x) = atom(Atom.Constant(x))
generated() = atom(Atom.Generated{Union{}}(first(counter)))
variable(x::Symbol) = atom(Atom.Variable{Union{}}(x))

value(e::Expression.Type{T}) where T = @match e begin
    Expression.Atom(Atom.Constant(x), _) => Maybe.Some(x)
    Expression.Normal(normal, cache, _) && if length(cache) == 1 && @match normal begin
        (
            Normal.Conjunction(clauses) ||
            Normal.Disjunction(clauses) &&
            if length(clauses) == 1 && length(only(clauses)) == 1 end
        ) => true
        _ => false
    end end => @match first(cache)[2] begin
        Either.Right(x) => Maybe.Some(x)
        _ => Maybe.Nothing{T}()
    end
    _ => Maybe.Nothing{T}()
end

function flatten(o, ps::Vector{Either.Type{Expression.Type{T}, Any}}) where T
    stack = copy(ps)
end

function flatten(o, ps::Vector{Either.Type{Expression.Type{T}, Any}}) where T
    input_stack = [o => ps]
    output_stack = Pair{Symbol, Vector{Either.Type{Int, Int}}}[]
    input_cache = Bijection{Int, Either.Type{Expression.Type{T}, Expression.Type{Union{}}}}()
    output_cache = Bijection{Int, Either.Type{Atom.Type{T}, Atom.Type{Union{}}}}()

    while !isempty(input_stack)
        _o, _ps = pop!(input_stack)
        qs = Either.Type{Int, Int}[]

        for p in _ps
            push!(qs, @match p begin
                Expression.Atom(x) => Either.Right{Int, Int}(
                    get!(() -> length(output_cache) + 1, output_cache, (@match x begin
                        Atom.Constant(_) => Either.Left
                        _ => Either.Right
                    end){T, Union{}}(x))
                )
                Expression.Error(x) => error(x)
                Expression.Nested(__o, __ps) => begin
                    push!(input_stack, __o => __ps)
                    Either.Left{Int, Int}(get!(() -> length(output_stack) + 1, input_cache, p))
                end
                Expression.Normal(x) => error()
            end)
        end

        push!(output_stack, _o => qs)
    end
end

# function flatten(o, ps::Vector{Either.Type{Expression.Type{T}, Any}}) where T
#     input_stack = [o => ps]
#     output_stack = Pair{Symbol, Vector{Either.Type{Int, Int}}}[]
#     input_cache = Bijection{Int, Either.Type{Expression.Type{T}, Expression.Type{Union{}}}}()
#     output_cache = Bijection{Int, Either.Type{Atom.Type{T}, Atom.Type{Union{}}}}()

#     while !isempty(input_stack)
#         _o, _ps = pop!(input_stack)
#         qs = Either.Type{Int, Int}[]

#         for p in _ps
#             push!(qs, @match p begin
#                 Expression.Atom(x) => begin
#                     LR = @match x begin
#                         Atom.Constant(_) => Either.Left
#                         _ => Either.Right
#                     end
#                     get!(() -> LR{Int, Int}(length(output_cache) + 1), output_cache, LR{T, Union{}}(x))
#                 end
#                 Expression.Error(x) => error(x)
#                 Expression.Nested(__o, __ps) => begin
#                     i = get!(() -> length(input_stack) + 1, input_cache, p)
#                     push!(qs, i)
#                 end
#                 Expression.Normal(x) => error()
#             end)
#         end

#         push!(output_stack, _o => qs)
#     end

#     output_stack, output_cache
# end




# @data Atom{T} begin
#     Constant(T)
#     Generated(Int)
#     Variable(Symbol)
# end

# @derive Atom[Eq, Hash, Show]

# copy(a::Atom.Type) = @match a begin
#     Atom.Constant(x) => Atom.Constant(copy(x))
#     _ => a
# end

# const Cache = Bijection{Int, Either.Type{Expression{Union{}}, Expression{T}}}
# const Clauses = Vector{Vector{Int}}
# const List = Vector{Pair{Symbol, Vector{Either.Type{Int, Int}}}}

# @data Compound begin
#     Conjunction(Clauses)
#     Constructed(List)
#     Disjunction(Clauses)
#     Negation(List)
# end

# @derive Compound[Eq, Hash, Show]

# copy(c::Compound.Type) = @match c begin
#     Compound.Conjunction(x) => Compound.Conjunction(copy(x))
#     Compound.Constructed(x) => Compound.Constructed(copy(x))
#     Compound.Disjunction(x) => Compound.Disjunction(copy(x))
#     Compound.Negation(x) => Compound.Negation(copy(x))
# end

# @data AbstractSyntaxTree{T} begin
#     Flattened(Cached.Type{T})
#     Nested(List)
# end

# @data AbstractSyntaxTree{T} begin
#     Atom(Atom.Type{T})
#     Compound(Compound.Type, Bijection{Int, Either.Type{Expression{Union{}}, Expression{T}}})
# end

# @derive AbstractSyntaxTree[Show]

# ==(ast::AbstractSyntaxTree.Type, _ast::AbstractSyntaxTree.Type) = @match (ast, _ast) begin
#     (AbstractSyntaxTree.Atom(x), AbstractSyntaxTree.Atom(_x)) => x == _x
#     (AbstractSyntaxTree.Compound(x, y), AbstractSyntaxTree.Compound(_x, _y)) => @match (x, _x) begin
#         (Compound.Negation(z), Compound.Negation(_z)) => flatten!(z, y) == flatten!(_z, _y)
#         _ => (x, y) == (_x, _y)
#     end
# end

# copy(ast::AbstractSyntaxTree.Type) = @match ast begin
#     AbstractSyntaxTree.Atom(x) => AbstractSyntaxTree.Atom(copy(x))
#     AbstractSyntaxTree.Compound(x, y) => AbstractSyntaxTree.Compound(copy(x), copy(y))
# end

# @data Expression{T} begin
#     AbstractSyntaxTree(AbstractSyntaxTree.Type{T})
#     Operator(Symbol)
#     Error(Vector{String})
# end

# @derive Expression[Eq, Hash]

# copy(e::Expression.Type) = @match e begin
#     Expression.AbstractSyntaxTree(x, y) => Expression.AbstractSyntaxTree(copy(x), copy(y))
#     Expression.Atom(x) => Expression.Atom(copy(x))
#     Expression.Error(x) => Expression.Error(copy(x))
#     _ => e
# end

# show(io::IO, m::MIME"text/plain", e::Expression.Type) = @match e begin
#     Expression.AbstractSyntaxTree(x, y) => print(io, Expression.AbstractSyntaxTree, "(", x, ", ", y, ")")
#     Expression.Atom(x) => @match x begin
#         Atom.Constant(y) => begin
#             print(io, "\$(")
#             show(io, x)
#             print(io, ")")
#         end
#         Atom.Generated(y) => print(io, '#', y)
#         Atom.Variable(y) => print(io, y)
#     end
#     Expression.Error(x) => show(io, m, x)
#     Expression.Operator(x) => print(io, x)
# end

# abstract_syntax_tree(x, y) = Expression.AbstractSyntaxTree(x, y)
# atom(x) = Expression.Atom(x)

# constant(x) = atom(Atom.Constant(x))
# operator(x::Symbol) = Expression.Operator{Union{}}(x)
# generated() = atom(Atom.Generated{Union{}}(first(counter)))
# variable(x::Symbol) = atom(Atom.Variable{Union{}}(x))

# value(e::Expression.Type{T}) where T = @match e begin
#     Expression.AbstractSyntaxTree(form, cache) && if length(cache) == 1 && @match form begin
#         (
#             Form.ConjunctionNormal(clauses) ||
#             Form.DisjunctionNormal(clauses) &&
#             if length(clauses) == 1 && length(only(clauses)) == 1 end
#         ) => true
#         _ => false
#     end end => @match first(cache)[2] begin
#         Either.Right(x) => Maybe.Some(x)
#         _ => Maybe.Nothing{T}()
#     end
#     Expression.Atom(Atom.Constant(x)) => Maybe.Some(x)
#     _ => Maybe.Nothing{T}()
# end

# normalize(o, p) = normalize!(o, copy(p))

# solutions(p) = solutions!(copy(p))

# function flatten!(list, cache)
#     if length(list) == 1
#         @match only(list) begin
#             Either.Left(_) => begin

#             end
#             _ => nothing
#         end
#     end

#     list, cache
# end


# function _flatten!(expressions, atoms)
#     empty!(expressions)

#     for (atom, index) in pairs(atoms)
#         expressions[index] = atom
#     end
# end

# function flatten!(e::Expression{T}) where T
#     operators, symbolics, constants = e.abstract_syntax_tree
#     n = length(operators)

#     if n == 1
#         _symbolics, _constants = Dict{Expression{Nothing}, Int}(), Dict{Expression{T}, Int}()
#         _operators = [pop!(operators)]

#         while !isempty(_operators)
#             operator, indices = pop!(_operators)

#             for (_, index) in Iterators.filter(first, indices)
#                 if signbit(index)
#                     p = _symbolics[index]
#                 else
#                 end
#             end
#         end

#         _flatten!(symbolics, _symbolics), _flatten!(constants, _constants)
#     end
# end

# function flatten!(f!, e)
#     es = get_application(e)
#     _es = @view es[2:end]
#     if arity(es[1][1]) < length(_es)
#         for (i, (_e, is)) in pairs(_es)
#             __e, k = f!(es, i, _e, is, _e.kind)
#             if k == application
#                 n = length(es)
#                 __es = get_application(__e)
#                 ___es = @view __es[2:end]
#                 es[i] = __es[1][1] => n:(n + length(___es))
#                 append!(es, ___es)
#             end
#         end
#     end
#     es
# end
# flatten!(e) = flatten!((_, _, e, _, k) -> (e, k), e)

# function map(f, e::Expression)
#     k = e.kind
#     if k == atom f(e)
#     elseif k == operator map(f, e())
#     elseif k == abstract_syntax_tree
#         e = copy(e)
#         flatten!(e) do es, i, e, is, k
#             if k == atom
#                 _e = f(e)
#                 es[i] = _e => is
#                 _e, _e.kind
#             else e, k
#             end
#         end
#         e
#     else e
#     end
# end

# function atoms(e::Expression)
#     k = e.kind
#     if k == atom; [e]
#     elseif k == operator atoms(e())
#     elseif k == abstract_syntax_tree filter(e -> e.kind == atom, flatten!(e))
#     else Expression[]
#     end
# end
