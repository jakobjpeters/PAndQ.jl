
using Test
using Documenter
using PAQ

DocMeta.setdocmeta!(
    PAQ,
    :DocTestSetup,
    :(
        using PAQ;
        @atom p q
    ),
    recursive=true
)

@testset "All" begin
    doctest(PAQ)
end

closure(f, xs) = filter(x -> f(x) in xs, xs)
# closure(f, xs::Set) = filter(x -> f(x) in xs, xs)
# closure(f, xs) = filter(x -> typeof(x) == typeof(f(x)), xs)
idempotent(f, xs) = all(x -> f(x, x) == x, xs)
associative(f, xs) = foldl(f, xs) == foldr(f, xs)
# identity(f, identity, xs) = all(x -> f(x, identity) == x, xs) # namespace?
# inverse(f, identity, xs) = 
total(f, xs) = try map(f, xs); return true catch; return false end
commutative(f, xs) = map(x -> all(map(y -> f(x, y) == f(y, x), xs)), xs)
# left_distributive(f, xs) = 
right_distributive(f, xs) = left_distributive(f, reverse(xs))
distributive(f, xs) = left_distributive(f, xs) && right_distributive(f, xs)

# algebraic data structures
magma(f, xs) = total(f, xs) && xs == closure(f, xs)
semigroup(f, xs) = magma(f, xs) && associative(f, xs)
unital_magma(f, identity, xs) = magma(f, xs) && identity(f, xs)
monoid(f, xs) = semigroup(f, xs) && identity(f, xs)
commutative_monoid(f, xs) = monoid(f, xs) && commutative(f, xs)
inverse_semigroup(f, xs) = semigroup(f, xs) && inverse(f, xs)
group(f, xs) = monoid(f, xs) && inverse(f, xs)
abelian_group(f, xs) = group(f, xs) && commutative(f, xs)

semi_ring(f, g, xs) = commutative(f, xs) && identity(f, xs) && monoid(g, xs)