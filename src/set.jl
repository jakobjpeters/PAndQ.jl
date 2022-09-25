
# import Base.∈, Base.∉, Base.⊆, Base.⊈, Base.⊇, Base.⊉, Base.∪, Base.∩

# set theory
# Base.:∪(A, B) = union(A, B)                 # \cup          - union of A and B
# Base.:∩(A, B) = intersect(A, B)             # \cap          - intersection of A and B
# Base.:∈(a, B) = a in B                      # \in           - a in B
# Base.:∉(a, B) = ¬a in B                     # \notin        - a not in B
# Base.:⊆ = issubset                          # \subseteq     - A subset of B
# Base.:⊈(A, B) = ¬⊆(A, B)                    # \nsubseteq    - A not subset of B
# Base.:⊇(A, B) = ⊆(B, A)                     # \supseteq     - A superset of B
# Base.:⊉(A, B) = ¬⊇(A, B)                    # \nsupseteq    - A not superset of B
# ⊂(A, B) = ⊆(A, B) ∧ length(A) < length(B)   # \subset       - A strict subset of B
# ⊄(A, B) = ¬⊂(A, B)                          # \nsubset      - A not strict subset of B
# ⊃(A, B) = ⊂(B, A)                           # \supset       - A superset of B
# ⊅(A, B) = ¬⊃(A, B)                          # \nsupset      - A not supserset of B

⨉ = Iterators.product # \bigtimes - cartesian product
