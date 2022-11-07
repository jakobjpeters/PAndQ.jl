
```@meta
DocTestSetup = quote
    using PAQ
end
```

# P∧Q.jl

Do you like logic? If so, you've come to the right package! Check out the [source code](https://github.com/jakobjpeters/PAQ.jl).

*Please mind your ```ps ∧ qs```.*

## Introduction

P∧Q.jl is designed to have an intuitive interface by enabling you to write code in logical symbols. This is thanks to Julia's support for [Unicode](https://docs.julialang.org/en/v1/manual/unicode-input/) and infix operators. Alternatively, every symbol has a written alias.

```jldoctest
julia> ¬⊤ ∧ ⊤

julia> @primitive p q

julia> r = p → q

julia> r

```

## Tutorial

Remember, every infix operator is a function.

```jldoctest
julia> @primitive p q

julia> p ∧ q == ∧(p, q) == and(p, q)
true
```

### Getting Started

### Examples
