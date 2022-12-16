var documenterSearchIndex = {"docs":
[{"location":"manual/abstract_types/#Abstract-Types","page":"Abstract Types","title":"Abstract Types","text":"","category":"section"},{"location":"manual/abstract_types/","page":"Abstract Types","title":"Abstract Types","text":"Language\nCompound\nOperator","category":"page"},{"location":"manual/abstract_types/#PAQ.Language","page":"Abstract Types","title":"PAQ.Language","text":"Language\n\nSet of well-formed logical formulae.\n\nCalling an instance of Language will return a vector of valid interpretations.\n\nSupertype of Primitive, Compound, and Truth. ```\n\n\n\n\n\n","category":"type"},{"location":"manual/abstract_types/#PAQ.Compound","page":"Abstract Types","title":"PAQ.Compound","text":"Compound <: Language\n\nCompound proposition.\n\nSubtype of Language. Supertype of Propositional.\n\n\n\n\n\n","category":"type"},{"location":"manual/abstract_types/#PAQ.Operator","page":"Abstract Types","title":"PAQ.Operator","text":"Operator\n\nSet of functions that operate on a logical Language.\n\nSupertype of Boolean.\n\n\n\n\n\n","category":"type"},{"location":"manual/internals/#Internals","page":"Internals","title":"Internals","text":"","category":"section"},{"location":"manual/internals/","page":"Internals","title":"Internals","text":"PAQ.interpret","category":"page"},{"location":"manual/internals/#PAQ.interpret","page":"Internals","title":"PAQ.interpret","text":"interpret(valuation, ϕ::Language)\n\nGiven a valuation function that maps from the Primitive propositions in ϕ to their respective Truth values, assign a truth value to ϕ.\n\nSee also Language.\n\njulia> mapping = Dict(p => ⊥, q => ⊤);\n\njulia> valuation = r -> mapping[r];\n\njulia> PAQ.interpret(valuation, p ∧ q)\n⊥\n\njulia> PAQ.interpret(valuation, p → q)\n⊤\n\n\n\n\n\n","category":"function"},{"location":"manual/propositional_logic/#Propositional-Logic","page":"Propositional Logic","title":"Propositional Logic","text":"","category":"section"},{"location":"manual/propositional_logic/","page":"Propositional Logic","title":"Propositional Logic","text":"Primitive\nBoolean\nNot\nAnd\nPropositional\nTruth\ntautology\ncontradiction\n@primitive\nget_primitives","category":"page"},{"location":"manual/propositional_logic/#PAQ.Primitive","page":"Propositional Logic","title":"PAQ.Primitive","text":"Primitive <: Language\nPrimitive([statement::String = \"\"])\n\nPrimitive proposition.\n\nSubtype of Language. See also Compound.\n\nExamples\n\njulia> p\nPrimitive(\"p\")\n\njulia> p()\n2-element Vector{Pair{Vector{Pair{Primitive, Truth}}}}:\n [Primitive(\"p\") => ⊤] => ⊤\n [Primitive(\"p\") => ⊥] => ⊥\n\n\n\n\n\n","category":"type"},{"location":"manual/propositional_logic/#PAQ.Boolean","page":"Propositional Logic","title":"PAQ.Boolean","text":"Boolean <: Operator\n\nSet of functionally complete logical connectives.\n\nSubtype of Operator. Supertype of Not and And. See also Boolean Operators.\n\n\n\n\n\n","category":"type"},{"location":"manual/propositional_logic/#PAQ.Not","page":"Propositional Logic","title":"PAQ.Not","text":"Not <: Boolean <: Operator\n\nSingleton type representing logical negation.\n\nSubtype of Boolean and Operator. See also And and Language.\n\nExamples\n\njulia> @truth_table PAQ.Not()(p)\n┌───────────┬────────────────┐\n│ p         │ (PAQ.Not())(p) │\n│ Primitive │ Propositional  │\n│ \"p\"       │                │\n├───────────┼────────────────┤\n│ ⊤         │ ⊥              │\n│ ⊥         │ ⊤              │\n└───────────┴────────────────┘\n\n\n\n\n\n","category":"type"},{"location":"manual/propositional_logic/#PAQ.And","page":"Propositional Logic","title":"PAQ.And","text":"And <: Boolean <: Operator\n\nSingleton type representing logical conjunction.\n\nSubtype of Boolean and Operator. See also Not and Language.\n\nExamples\n\njulia> @truth_table PAQ.And()(p, q)\n┌───────────┬───────────┬───────────────────┐\n│ p         │ q         │ (PAQ.And())(p, q) │\n│ Primitive │ Primitive │ Propositional     │\n│ \"p\"       │ \"q\"       │                   │\n├───────────┼───────────┼───────────────────┤\n│ ⊤         │ ⊤         │ ⊤                 │\n│ ⊤         │ ⊥         │ ⊥                 │\n├───────────┼───────────┼───────────────────┤\n│ ⊥         │ ⊤         │ ⊥                 │\n│ ⊥         │ ⊥         │ ⊥                 │\n└───────────┴───────────┴───────────────────┘\n\n\n\n\n\n","category":"type"},{"location":"manual/propositional_logic/#PAQ.Propositional","page":"Propositional Logic","title":"PAQ.Propositional","text":"Propositional{\n    L <: Union{\n        Primitive,\n        Tuple{Not, Compound},\n        Tuple{And, Compound, Compound}\n    }\n}(ϕ::L) <: Compound <: Language\n\nAbstract syntax tree representing a compound proposition.\n\nSubtype of Compound and Language. See also Primitive, Not, and And.\n\nExamples\n\njulia> p ∧ ¬p\nPropositional(\n  And(), Propositional(\n    Primitive(\"p\")\n  ) Propositional(\n    Not(), Propositional(\n      Primitive(\"p\")\n    ) \n  ) \n)\n\njulia> (p ∧ ¬p)()\n2-element Vector{Pair{Vector{Pair{Primitive, Truth}}, Truth{Val{:⊥}}}}:\n [Primitive(\"p\") => ⊤] => ⊥\n [Primitive(\"p\") => ⊥] => ⊥\n\njulia> (p → q) ∧ (p ← q) == ¬(p ⊻ q)\ntrue\n\n\n\n\n\n","category":"type"},{"location":"manual/propositional_logic/#PAQ.Truth","page":"Propositional Logic","title":"PAQ.Truth","text":"Truth{V <: Union{Val{:⊥}, Val{:⊤}}}(::V) <: Language\n\nContainer for the constants tautology and contradiction. Subtype of Language.\n\n\n\n\n\n","category":"type"},{"location":"manual/propositional_logic/#PAQ.tautology","page":"Propositional Logic","title":"PAQ.tautology","text":"⊤\ntautology\n\nA constant which is true in every possible interpretation.\n\nOne of two valid instances of Truth, the other instance being contradiction.\n\n⊤ can be typed by \\top<tab>.\n\nExamples\n\njulia> ¬⊤\n⊥\n\njulia> tautology()\n1-element Vector{Pair{Vector{Pair{Primitive, Truth}}, Truth{Val{:⊤}}}}:\n [] => ⊤\n\n\n\n\n\n","category":"constant"},{"location":"manual/propositional_logic/#PAQ.contradiction","page":"Propositional Logic","title":"PAQ.contradiction","text":"⊥\ncontradiction\n\nA constant which is false in every possible interpretation.\n\nOne of two valid instances of Truth, the other instance being tautology.\n\n⊥ can be typed by \\bot<tab>.\n\nExamples\n\njulia> ¬⊥\n⊤\n\njulia> contradiction()\n1-element Vector{Pair{Vector{Pair{Primitive, Truth}}, Truth{Val{:⊥}}}}:\n [] => ⊥\n\n\n\n\n\n","category":"constant"},{"location":"manual/propositional_logic/#PAQ.@primitive","page":"Propositional Logic","title":"PAQ.@primitive","text":"@primitive(ps...)\n\nInstantiates Primitive propositions.\n\nExamples\n\njulia> @primitive p q\n\njulia> p\nPrimitive(\"p\")\n\njulia> q\nPrimitive(\"q\")\n\n\n\n\n\n","category":"macro"},{"location":"manual/propositional_logic/#PAQ.get_primitives","page":"Propositional Logic","title":"PAQ.get_primitives","text":"get_primitives(ps::Language...)\n\nReturns a vector of Primitive propositions contained in p.\n\nNote that some primitives may optimized out of an expression, such as in p ∧ ⊥.\n\nSee also Language.\n\nExamples\n\njulia> get_primitives(p)\n1-element Vector{Primitive}:\n Primitive(\"p\")\n\njulia> get_primitives(p ∧ q, r)\n3-element Vector{Primitive}:\n Primitive(\"p\")\n Primitive(\"q\")\n Primitive(\"r\")\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#Semantics","page":"Semantics","title":"Semantics","text":"","category":"section"},{"location":"manual/semantics/","page":"Semantics","title":"Semantics","text":"is_tautology\nis_contradiction\nis_contingency\nis_satisfiable\nis_falsifiable\n==\n@truth_table","category":"page"},{"location":"manual/semantics/#PAQ.is_tautology","page":"Semantics","title":"PAQ.is_tautology","text":"is_tautology(p::Language)\n\nReturns a boolean on whether the given proposition is a tautology.\n\nThis function is equivalent to p == ⊤.\n\nSee also Language and ==.\n\nExamples\n\njulia> is_tautology(⊤)\ntrue\n\njulia> is_tautology(p)\nfalse\n\njulia> is_tautology(¬(p ∧ ¬p))\ntrue\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#PAQ.is_contradiction","page":"Semantics","title":"PAQ.is_contradiction","text":"is_contradiction(p::Language)\n\nReturns a boolean on whether the given proposition is a contradiction.\n\nThis function is equivalent to p == ⊥.\n\nSee also Language and ==.\n\nExamples\n\njulia> is_contradiction(⊥)\ntrue\n\njulia> is_contradiction(p)\nfalse\n\njulia> is_contradiction(p ∧ ¬p)\ntrue\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#PAQ.is_contingency","page":"Semantics","title":"PAQ.is_contingency","text":"is_contingency(p::Language)\n\nReturns a boolean on whether the given proposition is a contingency (neither a tautology or contradiction).\n\nWhile this function is equivalent to p != ⊤ && p != ⊥, is_contingency(p) is preferred because the former expression will give an incorrect result if p is not a subtype of Language.\n\nSee also Language.\n\nExamples\n\njulia> is_contingency(⊤)\nfalse\n\njulia> is_contingency(p ∧ ¬p)\nfalse\n\njulia> is_contingency(p)\ntrue\n\njulia> is_contingency(p ∧ q)\ntrue\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#PAQ.is_satisfiable","page":"Semantics","title":"PAQ.is_satisfiable","text":"is_satisfiable(p::Language)\n\nReturns a boolean on whether the given proposition is satisfiable (not a contradiction).\n\nWhile this function is equivalent to p != ⊥, is_satisfiable(p) is preferred because the former expression will give an incorrect result if p is not a subtype of Language.\n\nSee also Language.\n\nExamples\n\njulia> is_satisfiable(⊤)\ntrue\n\njulia> is_satisfiable(p ∧ ¬p)\nfalse\n\njulia> is_satisfiable(p)\ntrue\n\njulia> is_satisfiable(p ∧ q)\ntrue\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#PAQ.is_falsifiable","page":"Semantics","title":"PAQ.is_falsifiable","text":"is_falsifiable(p::Language)\n\nReturns a boolean on whether the given proposition is falsifiable (not a is_tautology).\n\nWhile this function is equivalent to p != ⊤, is_falsifiable(p) is preferred because the former expression will give an incorrect result if p is not a subtype of Language.\n\nSee also Language.\n\nExamples\n\njulia> is_falsifiable(⊥)\ntrue\n\njulia> is_falsifiable(¬(p ∧ ¬p))\nfalse\n\njulia> is_falsifiable(p)\ntrue\n\njulia> is_falsifiable(p ∧ q)\ntrue\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#Base.:==","page":"Semantics","title":"Base.:==","text":"p == q\n==(p::Language, q::Language)\nisequal(p::Language, q::Language)\n\nReturns a boolean indicating whether p and q are logically equivalent.\n\nSee also Language.\n\ninfo: Info\nThe ≡ symbol is sometimes used to represent logical equivalence. However, Julia uses ≡ as an alias for the builtin function === which cannot have methods added to it.\n\nExamples\n\njulia> p == ¬p\nfalse\n\njulia> julia> p ∨ q == ¬(¬q ∧ ¬p)\ntrue\n\njulia> isequal((p → q) ∧ (p ← q), ¬(p ⊻ q))\ntrue\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#PAQ.@truth_table","page":"Semantics","title":"PAQ.@truth_table","text":"@truth_table p\n@truth_table(ps...)\n\nPrint a truth table for the given propositions.\n\nThe first row of the header is the expression representing that column's proposition, the second row indicates that expression's type, and the third row identifies the statements for Primitive propositions.\n\ninfo: Info\nIf a variable contains a primitive, there is no expression to label that primitive. As such, the first row in the header will be blank. However, the identifying statement is still known and will be displayed in the third row. Use get_primitives to resolve this uncertainty.\n\nLogically equivalent propositions will be placed in the same column with their expressions in the header seperated by a comma.\n\nIn this context, ⊤ and ⊥ can be interpreted as true and false, respectively.\n\nSee also Language.\n\nExamples\n\njulia> @truth_table p∧q p→q\n┌───────────┬───────────┬───────────────┬───────────────┐\n│ p         │ q         │ p ∧ q         │ p → q         │\n│ Primitive │ Primitive │ Propositional │ Propositional │\n│ \"p\"       │ \"q\"       │               │               │\n├───────────┼───────────┼───────────────┼───────────────┤\n│ ⊤         │ ⊤         │ ⊤             │ ⊤             │\n│ ⊤         │ ⊥         │ ⊥             │ ⊥             │\n├───────────┼───────────┼───────────────┼───────────────┤\n│ ⊥         │ ⊤         │ ⊥             │ ⊤             │\n│ ⊥         │ ⊥         │ ⊥             │ ⊤             │\n└───────────┴───────────┴───────────────┴───────────────┘\n\n\n\n\n\n","category":"macro"},{"location":"manual/boolean_operators/#Boolean-Operators","page":"Boolean Operators","title":"Boolean Operators","text":"","category":"section"},{"location":"manual/boolean_operators/","page":"Boolean Operators","title":"Boolean Operators","text":"Name Symbol Tab completion\nnot ¬ \\neg\nand ∧ \\wedge\nor ∨ \\vee\nxor ⊻ \\xor\nxnor ↔ \\leftrightarrow\nif_then → \\rightarrow\nnot_if_then ↛ \\nrightarrow\nthen_if ← \\leftarrow\nnot_then_if ↚ \\nleftarrow","category":"page"},{"location":"manual/boolean_operators/","page":"Boolean Operators","title":"Boolean Operators","text":"not\nand\nBase.nand\nBase.nor\nor\nxor\nxnor\nif_then\nnot_if_then\nthen_if\nnot_then_if","category":"page"},{"location":"manual/boolean_operators/#PAQ.not","page":"Boolean Operators","title":"PAQ.not","text":"¬p\n¬(p)\nnot(p)\n\nLogical \"negation\" operator.\n\n¬ can be typed by \\neg<tab>.\n\nSee also Not.\n\nExamples\n\njulia> @truth_table ¬p\n┌───────────┬───────────────┐\n│ p         │ ¬p            │\n│ Primitive │ Propositional │\n│ \"p\"       │               │\n├───────────┼───────────────┤\n│ ⊤         │ ⊥             │\n│ ⊥         │ ⊤             │\n└───────────┴───────────────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/boolean_operators/#PAQ.and","page":"Boolean Operators","title":"PAQ.and","text":"p ∧ q\n∧(p, q)\nand(p::Language, q::Language)\n\nLogical \"conjunction\" operator.\n\n∧ can be typed by \\wedge<tab>.\n\nSee also And.\n\nExamples\n\njulia> @truth_table p ∧ q\n┌───────────┬───────────┬───────────────┐\n│ p         │ q         │ p ∧ q         │\n│ Primitive │ Primitive │ Propositional │\n│ \"p\"       │ \"q\"       │               │\n├───────────┼───────────┼───────────────┤\n│ ⊤         │ ⊤         │ ⊤             │\n│ ⊤         │ ⊥         │ ⊥             │\n├───────────┼───────────┼───────────────┤\n│ ⊥         │ ⊤         │ ⊥             │\n│ ⊥         │ ⊥         │ ⊥             │\n└───────────┴───────────┴───────────────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/boolean_operators/#PAQ.or","page":"Boolean Operators","title":"PAQ.or","text":"p ∨ q\n∨(p, q)\nor(p, q)\n\nLogical \"disjunction\" operator.\n\n∨ can be typed by \\vee<tab>.\n\nExamples\n\njulia> @truth_table p ∨ q\n┌───────────┬───────────┬───────────────┐\n│ p         │ q         │ p ∨ q         │\n│ Primitive │ Primitive │ Propositional │\n│ \"p\"       │ \"q\"       │               │\n├───────────┼───────────┼───────────────┤\n│ ⊤         │ ⊤         │ ⊤             │\n│ ⊤         │ ⊥         │ ⊤             │\n├───────────┼───────────┼───────────────┤\n│ ⊥         │ ⊤         │ ⊤             │\n│ ⊥         │ ⊥         │ ⊥             │\n└───────────┴───────────┴───────────────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/boolean_operators/#Base.xor","page":"Boolean Operators","title":"Base.xor","text":"p ⊻ q\n⊻(p, q)\nxor(p, q)\n\nLogical \"exclusive disjunction\" operator.\n\n⊻ can be typed by \\xor<tab>.\n\nExamples\n\njulia> @truth_table p ⊻ q\n┌───────────┬───────────┬───────────────┐\n│ p         │ q         │ p ⊻ q         │\n│ Primitive │ Primitive │ Propositional │\n│ \"p\"       │ \"q\"       │               │\n├───────────┼───────────┼───────────────┤\n│ ⊤         │ ⊤         │ ⊥             │\n│ ⊤         │ ⊥         │ ⊤             │\n├───────────┼───────────┼───────────────┤\n│ ⊥         │ ⊤         │ ⊤             │\n│ ⊥         │ ⊥         │ ⊥             │\n└───────────┴───────────┴───────────────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/boolean_operators/#PAQ.xnor","page":"Boolean Operators","title":"PAQ.xnor","text":"p ↔ q\n↔(p, q)\nxnor(p, q)\n\nLogical \"exclusive non-disjunction\" and \"bi-directional implication\" operator.\n\n↔ can be typed by \\leftrightarrow<tab>.\n\nExamples\n\njulia> @truth_table p ↔ q\n┌───────────┬───────────┬───────────────┐\n│ p         │ q         │ p ↔ q         │\n│ Primitive │ Primitive │ Propositional │\n│ \"p\"       │ \"q\"       │               │\n├───────────┼───────────┼───────────────┤\n│ ⊤         │ ⊤         │ ⊤             │\n│ ⊤         │ ⊥         │ ⊥             │\n├───────────┼───────────┼───────────────┤\n│ ⊥         │ ⊤         │ ⊥             │\n│ ⊥         │ ⊥         │ ⊤             │\n└───────────┴───────────┴───────────────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/boolean_operators/#PAQ.if_then","page":"Boolean Operators","title":"PAQ.if_then","text":"p → q\n→(p, q)\nif_then(p, q)\n\nLogical \"implication\" operator.\n\n→ can be typed by \\rightarrow<tab>.\n\nExamples\n\njulia> @truth_table p → q\n┌───────────┬───────────┬───────────────┐\n│ p         │ q         │ p → q         │\n│ Primitive │ Primitive │ Propositional │\n│ \"p\"       │ \"q\"       │               │\n├───────────┼───────────┼───────────────┤\n│ ⊤         │ ⊤         │ ⊤             │\n│ ⊤         │ ⊥         │ ⊥             │\n├───────────┼───────────┼───────────────┤\n│ ⊥         │ ⊤         │ ⊤             │\n│ ⊥         │ ⊥         │ ⊤             │\n└───────────┴───────────┴───────────────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/boolean_operators/#PAQ.not_if_then","page":"Boolean Operators","title":"PAQ.not_if_then","text":"p ↛ q\n↛(p, q)\nnot_if_then(p, q)\n\nLogical non-implication operator.\n\n↛ can be typed by \\nrightarrow<tab>.\n\nExamples\n\njulia> @truth_table p ↛ q\n┌───────────┬───────────┬───────────────┐\n│ p         │ q         │ p ↛ q         │\n│ Primitive │ Primitive │ Propositional │\n│ \"p\"       │ \"q\"       │               │\n├───────────┼───────────┼───────────────┤\n│ ⊤         │ ⊤         │ ⊥             │\n│ ⊤         │ ⊥         │ ⊤             │\n├───────────┼───────────┼───────────────┤\n│ ⊥         │ ⊤         │ ⊥             │\n│ ⊥         │ ⊥         │ ⊥             │\n└───────────┴───────────┴───────────────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/boolean_operators/#PAQ.then_if","page":"Boolean Operators","title":"PAQ.then_if","text":"p ← q\n←(p, q)\nthen_if(p, q)\n\nLogical \"converse implication\" operator.\n\n← can be typed by \\leftarrow<tab>.\n\nExamples\n\njulia> @truth_table p ← q\n┌───────────┬───────────┬───────────────┐\n│ p         │ q         │ p ← q         │\n│ Primitive │ Primitive │ Propositional │\n│ \"p\"       │ \"q\"       │               │\n├───────────┼───────────┼───────────────┤\n│ ⊤         │ ⊤         │ ⊤             │\n│ ⊤         │ ⊥         │ ⊤             │\n├───────────┼───────────┼───────────────┤\n│ ⊥         │ ⊤         │ ⊥             │\n│ ⊥         │ ⊥         │ ⊤             │\n└───────────┴───────────┴───────────────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/boolean_operators/#PAQ.not_then_if","page":"Boolean Operators","title":"PAQ.not_then_if","text":"p ↚ q\n↚(p, q)\nnot_then_if(p, q)\n\nLogical \"converse non-implication\" operator.\n\n↚ can be typed by \\nleftarrow<tab>.\n\nExamples\n\njulia> @truth_table p ↚ q\n┌───────────┬───────────┬───────────────┐\n│ p         │ q         │ p ↚ q         │\n│ Primitive │ Primitive │ Propositional │\n│ \"p\"       │ \"q\"       │               │\n├───────────┼───────────┼───────────────┤\n│ ⊤         │ ⊤         │ ⊥             │\n│ ⊤         │ ⊥         │ ⊥             │\n├───────────┼───────────┼───────────────┤\n│ ⊥         │ ⊤         │ ⊤             │\n│ ⊥         │ ⊥         │ ⊥             │\n└───────────┴───────────┴───────────────┘\n\n\n\n\n\n","category":"function"},{"location":"","page":"Home","title":"Home","text":"DocTestSetup = quote\n    using PAQ\nend","category":"page"},{"location":"#PQ.jl","page":"Home","title":"P∧Q.jl","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Do you like logic? If so, then you've come to the right package! Check out the source code.","category":"page"},{"location":"#Introduction","page":"Home","title":"Introduction","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"P∧Q.jl implements propositional logic (with more to come). It is designed to have an intuitive interface by enabling you to write and evaluate logical statements symbolically. This is thanks to Julia's support for Unicode and infix operators. Alternatively, every symbol has a written alias.","category":"page"},{"location":"#Installation","page":"Home","title":"Installation","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"julia> import Pkg\n\njulia> Pkg.add(url = \"https://github.com/jakobjpeters/PAQ.jl\")\n\njulia> using PAQ","category":"page"},{"location":"#Showcase","page":"Home","title":"Showcase","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"julia> ¬⊥\n⊤\n\njulia> @primitive p q\n\njulia> r = ¬p\nPropositional(\n  Not(), Propositional(\n    Primitive(\"p\")\n  ) \n)\n\njulia> ¬r\nPrimitive(\"p\")\n\njulia> p ∨ ⊤\n⊤\n\njulia> @truth_table ¬p r p → q\n┌───────────┬───────────┬───────────────┬───────────────┐\n│ p         │ q         │ ¬p, r         │ p → q         │\n│ Primitive │ Primitive │ Propositional │ Propositional │\n│ \"p\"       │ \"q\"       │               │               │\n├───────────┼───────────┼───────────────┼───────────────┤\n│ ⊤         │ ⊤         │ ⊥             │ ⊤             │\n│ ⊤         │ ⊥         │ ⊥             │ ⊥             │\n├───────────┼───────────┼───────────────┼───────────────┤\n│ ⊥         │ ⊤         │ ⊤             │ ⊤             │\n│ ⊥         │ ⊥         │ ⊤             │ ⊤             │\n└───────────┴───────────┴───────────────┴───────────────┘","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"DocTestSetup = quote\n    using PAQ\n    @primitive p q r\nend","category":"page"},{"location":"tutorial/#Tutorial","page":"Tutorial","title":"Tutorial","text":"","category":"section"},{"location":"tutorial/#Propositional-Logic","page":"Tutorial","title":"Propositional Logic","text":"","category":"section"},{"location":"tutorial/#Primitive-Propositions","page":"Tutorial","title":"Primitive Propositions","text":"","category":"section"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"A primitive proposition is a statement that can be true or false. For example, the statement \"Logic is fun\" may be true for you but false for someone else. Primitive propositions can be expressed as:","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"julia> p = Primitive(\"Logic is fun\")\nPrimitive(\"Logic is fun\")\n\njulia> q = Primitive(\"Julia is awesome\")\nPrimitive(\"Julia is awesome\")","category":"page"},{"location":"tutorial/#Compound-Propositions","page":"Tutorial","title":"Compound Propositions","text":"","category":"section"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Since p can be true or false, we can form other logical statements that depends on p's truth value. These statements use logical connectives and are called Compound propositions. To express the proposition that \"Logic is not fun\", use the logical not connective: not(p) or ¬p.  If p's truth value is true, then ¬p's truth value is false, and vice versa. A helpful tool to check a statement's truth values is @truth_table.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"julia> @truth_table ¬p\n┌───────────┬───────────────┐\n│ p         │ ¬p            │\n│ Primitive │ Propositional │\n│ \"p\"       │               │\n├───────────┼───────────────┤\n│ ⊤         │ ⊥             │\n│ ⊥         │ ⊤             │\n└───────────┴───────────────┘","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"info: Info\nFor now, think of the symbols ⊤ and ⊥ as true and false, respectively. An exact definition of them will be given in a couple of paragraphs.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Statements can also depend on multiple primitive propositions. The logical and connective is true when both p and q are true and is false otherwise. This is expressed as and(p, q), ∧(p, q), or p ∧ q. Repeatedly combining the connectives not and and can produce any possible truth table. As such, they are referred to as functionally complete. For example, the connective or is equivalent to ¬(¬p ∧ ¬q).","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"julia> @truth_table or(p, q) ¬(¬p ∧ ¬q)\n┌───────────┬───────────┬──────────────────────┐\n│ p         │ q         │ or(p, q), ¬(¬p ∧ ¬q) │\n│ Primitive │ Primitive │ Propositional        │\n│ \"p\"       │ \"q\"       │                      │\n├───────────┼───────────┼──────────────────────┤\n│ ⊤         │ ⊤         │ ⊤                    │\n│ ⊤         │ ⊥         │ ⊤                    │\n├───────────┼───────────┼──────────────────────┤\n│ ⊥         │ ⊤         │ ⊤                    │\n│ ⊥         │ ⊥         │ ⊥                    │\n└───────────┴───────────┴──────────────────────┘","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"info: Info\nThe first two cells of each row in this table is an interpretation, which allows the truth value of the corresponding last cell to be determined. More generally, interpretations are an assignment of meaning to logical symbols. A function that maps logical symbols or formulae to their meaning is called a valuation function.","category":"page"},{"location":"tutorial/#Truth-Values","page":"Tutorial","title":"Truth Values","text":"","category":"section"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Consider the proposition p ∧ ¬p. Using the earlier example, this states that both \"Logic is fun\" and \"Logic is not fun\". Since these statements are mutually exclusive, their conjunction forms a contradiction. A contradiction is a statement that is false in every possible interpretation. In other words, the statement p ∧ ¬p is false regardless of whether p's truth value is true or false. A contradiction can be expressed as contradiction or with the symbol ⊥. The negation of a contradiction, in this case ¬(p ∧ ¬p), results in a statement that is true in every possible interpretation. This is called a tautology and can be expressed as tautology or with the symbol ⊤. Contradiction and tautology symbols are also be used to express the concepts of true and false, respectively.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"info: Info\nNote that ⊤ is a Unicode symbol, not an uppercase \"t\". The documentation for each symbol provides instructions on how to type it. For example, ⊤ can be typed by \\top<tab>. See also Julia's documentation on Unicode Input.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"julia> ¬⊥\n⊤\n\njulia> p ∧ ⊤ # identity law\nPrimitive(\"p\")\n\njulia> p ∧ ⊥ # domination law\n⊥","category":"page"},{"location":"tutorial/#Implementation","page":"Tutorial","title":"Implementation","text":"","category":"section"},{"location":"tutorial/#Types","page":"Tutorial","title":"Types","text":"","category":"section"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"using PAQ # hide\nusing InteractiveUtils # hide\nusing AbstractTrees # hide\n\nAbstractTrees.children(x::Type) = InteractiveUtils.subtypes(x) # hide\nprint_tree(Language) # hide","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"In Backus-Naur Form (BNF), Propositional is defined inductively as:","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"ϕ ::= p | ¬ψ | ψ ∧ ψ","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Since we may want to refer to compound statements defined differently, ψ has the abstract type Compound rather than being a Propositional.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"Remember, every infix operator is a function. They also each have a written alias.","category":"page"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"julia> p ∧ q === ∧(p, q) === and(p, q)\ntrue","category":"page"},{"location":"tutorial/#Minimization","page":"Tutorial","title":"Minimization","text":"","category":"section"},{"location":"tutorial/#Order-of-Operations","page":"Tutorial","title":"Order of Operations","text":"","category":"section"},{"location":"tutorial/","page":"Tutorial","title":"Tutorial","text":"<!– associativity –>","category":"page"}]
}
