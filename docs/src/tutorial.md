
```@meta
DocTestSetup = quote
    using PAQ
    @primitive p q r
end
```

# Tutorial

## something

Remember, every infix operator is a function. They also each have a written alias.

```jldoctest index
julia> p ∧ q == ∧(p, q) == and(p, q)
true
```

## Minimization

## Order of Operations
