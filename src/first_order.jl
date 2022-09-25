
# ∀(f, xs) = all(map(f, xs))  # \forall
# ∃(f, xs) = any(map(f, xs))  # \exists
# ∄(f, xs) = ¬∃(f, xs)        # \nexists

# ∏ = reduce(⨉) # \prod
# ∑ = sum
# ⋀(ps) = ∀(∧, ps) # \bigwedge
# ⋁(ps) = ∀(∨, ps) # \bigvee
# ⋂ # \bigcap
# ⋃ # \bigcup

# function ⊥(f, p) end
# function ⊤(f, p) end

# contingency(p) = ⊥(p) ⊽ ⊤(p)
# satisfiable(p) = ¬⊥(p)

# @infix function Base.:≡(ps) = ∀(p -> ⊤(↔, p), ps) # \equiv