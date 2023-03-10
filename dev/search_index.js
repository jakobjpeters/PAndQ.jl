var documenterSearchIndex = {"docs":
[{"location":"manual/operators/","page":"Operators","title":"Operators","text":"DocTestSetup = :(using PAQ)","category":"page"},{"location":"manual/operators/#Boolean-Operators","page":"Operators","title":"Boolean Operators","text":"","category":"section"},{"location":"manual/operators/","page":"Operators","title":"Operators","text":"Every possible truth table can be constructed with the functionally complete set of operators not and and. For convenience, all sixteen of them have been prepared. There are ten binary operators, with the remaining six being expressed with truth values, individual propositions, and the unary not operator.","category":"page"},{"location":"manual/operators/","page":"Operators","title":"Operators","text":"julia> @truth_table ⊤ ⊥ ¬p ¬q\n┌────────┬────────┬──────┬──────┬─────────┬─────────┐\n│ ⊤      │ ⊥      │ p    │ q    │ ¬p      │ ¬q      │\n│ Clause │ Clause │ Atom │ Atom │ Literal │ Literal │\n├────────┼────────┼──────┼──────┼─────────┼─────────┤\n│ ⊤      │ ⊥      │ ⊤    │ ⊤    │ ⊥       │ ⊥       │\n│ ⊤      │ ⊥      │ ⊥    │ ⊤    │ ⊤       │ ⊥       │\n├────────┼────────┼──────┼──────┼─────────┼─────────┤\n│ ⊤      │ ⊥      │ ⊤    │ ⊥    │ ⊥       │ ⊤       │\n│ ⊤      │ ⊥      │ ⊥    │ ⊥    │ ⊤       │ ⊤       │\n└────────┴────────┴──────┴──────┴─────────┴─────────┘","category":"page"},{"location":"manual/operators/","page":"Operators","title":"Operators","text":"Typing symbols with tab completion is performed by typing \\, followed by the given characters, and then the [TAB] key. For example, ⊥ is typed with \\bot[TAB]. See also Julia's documentation on Tab Completion and Unicode Input.","category":"page"},{"location":"manual/operators/","page":"Operators","title":"Operators","text":"Operator associativity determines how operators with the same precedence group their operands. For example, ∧ is left associative. Therefore, p ∧ q ∧ r is equivalent to (p ∧ q) ∧ r. Operator precedence determines how expressions with distinct operators are grouped together. Higher precedence operators will group their operands before lower precedence operators. For example, ∧ has a higher precedence than ∨. Therefore, p ∨ q ∧ r is equivalent to p ∨ (q ∧ r), even though both operators are left associative. See also Julia's documentation on Operator Precedence and Associativity.","category":"page"},{"location":"manual/operators/","page":"Operators","title":"Operators","text":"info: Info\n== has a precedence of 7, which is higher than that of several binary operators. For some cases, you may need to use parentheses. For example, @p p → q == r will error, but @p (p → q) == r will correctly return false.","category":"page"},{"location":"manual/operators/","page":"Operators","title":"Operators","text":"Name Symbol Tab completion Associativity Precedence\ntautology ⊤ \\top none 0\ncontradiction ⊥ \\bot none 0\nnot ¬ \\neg right 0\nand ∧ \\wedge left 12\nnand ⊼ \\nand left 12\nnor ⊽ \\nor left 11\nor ∨ \\vee left 11\nxor ⊻ \\xor left 11\nxnor ↔ \\leftrightarrow right 4\nimply → \\rightarrow right 4\nnot_imply ↛ \\nrightarrow right 4\nconverse_imply ← \\leftarrow right 4\nnot_converse_imply ↚ \\nleftarrow right 4","category":"page"},{"location":"manual/operators/","page":"Operators","title":"Operators","text":"tautology\ncontradiction\nidentity\nnot\nand\nnand\nnor\nor\nxor\nxnor\nimply\nnot_imply\nconverse_imply\nnot_converse_imply","category":"page"},{"location":"manual/operators/#PAQ.tautology","page":"Operators","title":"PAQ.tautology","text":"⊤\n⊤()\ntautology\ntautology()\n\nLogical true operator.\n\n⊤ can be typed by \\top<tab>.\n\nExamples\n\njulia> ⊤()\ntautology (generic function with 1 method)\n\njulia> @truth_table ⊤\n┌────────┐\n│ ⊤      │\n│ Clause │\n├────────┤\n│ ⊤      │\n└────────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/operators/#PAQ.contradiction","page":"Operators","title":"PAQ.contradiction","text":"⊥\n⊥()\ncontradiction\ncontradiction()\n\nLogical false operator.\n\n⊥ can be typed by \\bot<tab>.\n\nExamples\n\njulia> ⊥()\ncontradiction (generic function with 1 method)\n\njulia> @truth_table ⊥\n┌────────┐\n│ ⊥      │\n│ Clause │\n├────────┤\n│ ⊥      │\n└────────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/operators/#Base.identity","page":"Operators","title":"Base.identity","text":"identity(::Proposition)\n\nLogical identity operator.\n\nExamples\n\njulia> @truth_table p\n┌──────┐\n│ p    │\n│ Atom │\n├──────┤\n│ ⊤    │\n│ ⊥    │\n└──────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/operators/#PAQ.not","page":"Operators","title":"PAQ.not","text":"¬p\n¬(p)\nnot(p)\n\nLogical negation operator.\n\n¬ can be typed by \\neg<tab>.\n\nExamples\n\njulia> @truth_table ¬p\n┌──────┬─────────┐\n│ p    │ ¬p      │\n│ Atom │ Literal │\n├──────┼─────────┤\n│ ⊤    │ ⊥       │\n│ ⊥    │ ⊤       │\n└──────┴─────────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/operators/#PAQ.and","page":"Operators","title":"PAQ.and","text":"p ∧ q\n∧(p, q)\nand(p, q)\n\nLogical conjunction operator.\n\n∧ can be typed by \\wedge<tab>.\n\nExamples\n\njulia> @truth_table p ∧ q\n┌──────┬──────┬───────┐\n│ p    │ q    │ p ∧ q │\n│ Atom │ Atom │ Tree  │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊤    │ ⊤     │\n│ ⊥    │ ⊤    │ ⊥     │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊥    │ ⊥     │\n│ ⊥    │ ⊥    │ ⊥     │\n└──────┴──────┴───────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/operators/#Base.nand","page":"Operators","title":"Base.nand","text":"p ⊼ q\n⊼(p, q)\nnand(p, q)\n\nLogical non-conjunction operator.\n\n⊼ can be typed by \\nand<tab>.\n\nExamples\n\njulia> @truth_table p ⊼ q\n┌──────┬──────┬───────┐\n│ p    │ q    │ p ⊼ q │\n│ Atom │ Atom │ Tree  │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊤    │ ⊥     │\n│ ⊥    │ ⊤    │ ⊤     │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊥    │ ⊤     │\n│ ⊥    │ ⊥    │ ⊤     │\n└──────┴──────┴───────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/operators/#Base.nor","page":"Operators","title":"Base.nor","text":"p ⊽ q\n⊽(p, q)\nnor(p, q)\n\nLogical non-disjunction operator.\n\n⊽ can be typed by \\nor<tab>.\n\nExamples\n\njulia> @truth_table p ⊽ q\n┌──────┬──────┬───────┐\n│ p    │ q    │ p ⊽ q │\n│ Atom │ Atom │ Tree  │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊤    │ ⊥     │\n│ ⊥    │ ⊤    │ ⊥     │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊥    │ ⊥     │\n│ ⊥    │ ⊥    │ ⊤     │\n└──────┴──────┴───────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/operators/#PAQ.or","page":"Operators","title":"PAQ.or","text":"p ∨ q\n∨(p, q)\nor(p, q)\n\nLogical disjunction operator.\n\n∨ can be typed by \\vee<tab>.\n\nExamples\n\njulia> @truth_table p ∨ q\n┌──────┬──────┬───────┐\n│ p    │ q    │ p ∨ q │\n│ Atom │ Atom │ Tree  │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊤    │ ⊤     │\n│ ⊥    │ ⊤    │ ⊤     │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊥    │ ⊤     │\n│ ⊥    │ ⊥    │ ⊥     │\n└──────┴──────┴───────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/operators/#Base.xor","page":"Operators","title":"Base.xor","text":"p ⊻ q\n⊻(p, q)\nxor(p, q)\n\nLogical exclusive disjunction operator.\n\n⊻ can be typed by \\xor<tab>.\n\nExamples\n\njulia> @truth_table p ⊻ q\n┌──────┬──────┬───────┐\n│ p    │ q    │ p ⊻ q │\n│ Atom │ Atom │ Tree  │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊤    │ ⊥     │\n│ ⊥    │ ⊤    │ ⊤     │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊥    │ ⊤     │\n│ ⊥    │ ⊥    │ ⊥     │\n└──────┴──────┴───────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/operators/#PAQ.xnor","page":"Operators","title":"PAQ.xnor","text":"p ↔ q\n↔(p, q)\nxnor(p, q)\n\nLogical exclusive non-disjunction and biconditional operator.\n\n↔ can be typed by \\leftrightarrow<tab>.\n\nExamples\n\njulia> @truth_table p ↔ q\n┌──────┬──────┬───────┐\n│ p    │ q    │ p ↔ q │\n│ Atom │ Atom │ Tree  │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊤    │ ⊤     │\n│ ⊥    │ ⊤    │ ⊥     │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊥    │ ⊥     │\n│ ⊥    │ ⊥    │ ⊤     │\n└──────┴──────┴───────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/operators/#PAQ.imply","page":"Operators","title":"PAQ.imply","text":"p → q\n→(p, q)\nimply(p, q)\n\nLogical implication operator.\n\n→ can be typed by \\rightarrow<tab>.\n\nExamples\n\njulia> @truth_table p → q\n┌──────┬──────┬───────┐\n│ p    │ q    │ p → q │\n│ Atom │ Atom │ Tree  │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊤    │ ⊤     │\n│ ⊥    │ ⊤    │ ⊤     │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊥    │ ⊥     │\n│ ⊥    │ ⊥    │ ⊤     │\n└──────┴──────┴───────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/operators/#PAQ.not_imply","page":"Operators","title":"PAQ.not_imply","text":"p ↛ q\n↛(p, q)\nnot_imply(p, q)\n\nLogical non-implication operator.\n\n↛ can be typed by \\nrightarrow<tab>.\n\nExamples\n\njulia> @truth_table p ↛ q\n┌──────┬──────┬───────┐\n│ p    │ q    │ p ↛ q │\n│ Atom │ Atom │ Tree  │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊤    │ ⊥     │\n│ ⊥    │ ⊤    │ ⊥     │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊥    │ ⊤     │\n│ ⊥    │ ⊥    │ ⊥     │\n└──────┴──────┴───────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/operators/#PAQ.converse_imply","page":"Operators","title":"PAQ.converse_imply","text":"p ← q\n←(p, q)\nconverse_imply(p, q)\n\nLogical converse implication operator.\n\n← can be typed by \\leftarrow<tab>.\n\nExamples\n\njulia> @truth_table p ← q\n┌──────┬──────┬───────┐\n│ p    │ q    │ p ← q │\n│ Atom │ Atom │ Tree  │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊤    │ ⊤     │\n│ ⊥    │ ⊤    │ ⊥     │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊥    │ ⊤     │\n│ ⊥    │ ⊥    │ ⊤     │\n└──────┴──────┴───────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/operators/#PAQ.not_converse_imply","page":"Operators","title":"PAQ.not_converse_imply","text":"p ↚ q\n↚(p, q)\nnot_converse_imply(p, q)\n\nLogical converse non-implication operator.\n\n↚ can be typed by \\nleftarrow<tab>.\n\nExamples\n\njulia> @truth_table p ↚ q\n┌──────┬──────┬───────┐\n│ p    │ q    │ p ↚ q │\n│ Atom │ Atom │ Tree  │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊤    │ ⊥     │\n│ ⊥    │ ⊤    │ ⊤     │\n├──────┼──────┼───────┤\n│ ⊤    │ ⊥    │ ⊥     │\n│ ⊥    │ ⊥    │ ⊥     │\n└──────┴──────┴───────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/propositions/#Propositional-Logic","page":"Propositions","title":"Propositional Logic","text":"","category":"section"},{"location":"manual/propositions/","page":"Propositions","title":"Propositions","text":"tip: Tip\nPropositions can be converted into different, but logically equivalent forms (see also ==). For example, ⊥ == Valuation(⊥) == Tree(⊥) == Clause(⊥). However, not all forms are Expressively complete. Otherwise, the conversion may throw an exception. For example, there is no way to represent Literal(⊥).","category":"page"},{"location":"manual/propositions/","page":"Propositions","title":"Propositions","text":"import AbstractTrees: children # hide\nusing AbstractTrees: print_tree # hide\nusing InteractiveUtils: subtypes # hide\nusing PAQ: Proposition # hide\n\nchildren(x::Type) = subtypes(x) # hide\nprint_tree(Proposition) # hide","category":"page"},{"location":"manual/propositions/","page":"Propositions","title":"Propositions","text":"Proposition\nCompound\nExpressive\nAtom\nLiteral\nClause\nNormal\nValuation\nTree","category":"page"},{"location":"manual/propositions/#PAQ.Proposition","page":"Propositions","title":"PAQ.Proposition","text":"Proposition\n\nThe set of well-formed logical formulae.\n\nSupertype of Atom and Compound.\n\n\n\n\n\n","category":"type"},{"location":"manual/propositions/#PAQ.Compound","page":"Propositions","title":"PAQ.Compound","text":"Compound <: Proposition\n\nA proposition composed from connecting Atomicpropositions with BooleanOperators.\n\nSubtype of Proposition. Supertype of Literal, Clause, and Expressive.\n\n\n\n\n\n","category":"type"},{"location":"manual/propositions/#PAQ.Expressive","page":"Propositions","title":"PAQ.Expressive","text":"Expressive <: Compound\n\nA proposition that is expressively complete.\n\nSubtype of Compound. Supertype of Valuation, Tree, andNormal.\n\n\n\n\n\n","category":"type"},{"location":"manual/propositions/#PAQ.Atom","page":"Propositions","title":"PAQ.Atom","text":"Atom{SS <: Union{String, Symbol}} <: Proposition\nAtom(::SS = :_)\nAtom(::AtomicProposition)\n\nA proposition with no deeper propositional structure.\n\nA string argument can be thought of as a specific statement,while a symbol can be variable. However, the only builtin difference between these are how they pretty-print. An atom with a string argument will be encompassed by quotation marks, while an atom with a symbol argument will only show the symbol's characters.\n\ntip: Tip\nUse @atoms or @p as a shortcut to define atoms or instantiate them inline, respectively.\n\ninfo: Info\nThe default parameter :_ represents an Atom with an unspecified statement. For example, Tree(⊥) returns Tree(and(Atom(:_), not(Atom(:_))), which pretty-prints as _ ∧ ¬_. The underscore is a special case; it is not idiomatic to use for most purposes.\n\nSubtype of Proposition. See also AtomicProposition.\n\nExamples\n\njulia> Atom(:p)\nAtom:\n p\n\njulia> Atom(\"Logic is fun\")\nAtom:\n \"Logic is fun\"\n\n\n\n\n\n","category":"type"},{"location":"manual/propositions/#PAQ.Literal","page":"Propositions","title":"PAQ.Literal","text":"Literal{UO <: UnaryOperator} <: Compound\nLiteral(::UO, ::Atom)\nLiteral(::LiteralProposition)\n\nA proposition represented by an atomic formula or its negation.\n\nSubtype of Compound. See also UnaryOperator, Atom, and LiteralProposition.\n\nExamples\n\njulia> r = @p ¬p\nLiteral:\n ¬p\n\njulia> ¬r\nAtom:\n p\n\n\n\n\n\n","category":"type"},{"location":"manual/propositions/#PAQ.Clause","page":"Propositions","title":"PAQ.Clause","text":"Clause{AO <: AndOr, L <: Literal} <: Compound\nClause(::AO, ::Vector = Literal[])\nClause(::AO, ps...)\n\nA proposition represented as either a conjunction or disjunction of literals.\n\ninfo: Info\nAn empty clause is logically equivalent to the identity element of it's binary operator.\n\nSee also Literal. Subtype of Compound.\n\nExamples\n\njulia> Clause(and)\nClause:\n ⊥\n\njulia> @p Clause(and, p, q)\nClause:\n p ∧ q\n\njulia> @p Clause(or, [¬p, q])\nClause:\n ¬p ∨ q\n\n\n\n\n\n","category":"type"},{"location":"manual/propositions/#PAQ.Normal","page":"Propositions","title":"PAQ.Normal","text":"Normal{AO <: AndOr, C <: Clause} <: Expressive\nNormal(::A, ::Vector{C} = C[]) where {A <: typeof(and), C <: Clause{typeof(or)}}\nNormal(::O, ::Vector{C} = C[]) where {O <: typeof(or), C <: Clause{typeof(and)}}\nNormal(::AO, ps...)\n\nA proposition represented in conjunctive or disjunctive normal form.\n\ninfo: Info\nAn empty normal form is logically equivalent to the identity element of it's binary operator.\n\nSubtype of Expressive.\n\nExamples\n\njulia> s = @p Normal(and, Clause(or, p, q), Clause(or, ¬r))\nNormal:\n (p ∨ q) ∧ (¬r)\n\njulia> ¬s\nNormal:\n (¬p ∧ ¬q) ∨ (r)\n\n\n\n\n\n","category":"type"},{"location":"manual/propositions/#PAQ.Valuation","page":"Propositions","title":"PAQ.Valuation","text":"Valuation{P <: Pair} <: Expressive\nValuation(::Vector{P})\nValuation(p)\n\nProposition represented by a vector of interpretations.\n\nSubtype of Expressive.\n\nExamples\n\njulia> @p Valuation(p ∧ q)\nValuation:\n [p => ⊤, q => ⊤] => ⊤\n [p => ⊥, q => ⊤] => ⊥\n [p => ⊤, q => ⊥] => ⊥\n [p => ⊥, q => ⊥] => ⊥\n\njulia> Valuation(⊥)\nValuation:\n [] => ⊥\n\n\n\n\n\n","category":"type"},{"location":"manual/propositions/#PAQ.Tree","page":"Propositions","title":"PAQ.Tree","text":"Tree{\n    O <: BooleanOperator,\n    P <: Union{Tuple{Proposition}, Tuple{Proposition, Proposition}}\n} <: Expressive\nTree(::UnaryOperator, ::Atom)\nTree(::BinaryOperator, ::Tree, ::Tree)\nTree(x)\n\nA proposition represented by an abstract syntax tree.\n\nSubtype of Expressive.\n\nExamples\n\njulia> r = @p p ⊻ q\nTree:\n p ⊻ q\n\njulia> @p ¬r → s\nTree:\n (p ↔ q) → s\n\n\n\n\n\n","category":"type"},{"location":"manual/semantics/#Semantics","page":"Semantics","title":"Semantics","text":"","category":"section"},{"location":"manual/semantics/","page":"Semantics","title":"Semantics","text":"interpret\n==\nis_tautology\nis_contradiction\nis_truth\nis_contingency\nis_satisfiable\nis_falsifiable\nsolve\nconverse\ndual","category":"page"},{"location":"manual/semantics/#PAQ.interpret","page":"Semantics","title":"PAQ.interpret","text":"interpret(p, valuation...)\n\nAssign a truth value value to p.\n\nLet p be a Proposition. Let valuation be a function, callable object, dictionary, or any number of Pairs that map from atomic propositions in p to their respective truth values.\n\nCalling p with an incomplete mapping will partially interpret p. This returns a Proposition of the same type as p?*? that is independent from every Atoms in valuation.\n\nwarning: Warning\nIf valuation does not return a Truth or errors, \n\nExamples\n\n\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#Base.:==","page":"Semantics","title":"Base.:==","text":"p == q\n==(p, q)\nisequal(p, q)\n\nReturns a boolean indicating whether p and q are logically equivalent.\n\ninfo: Info\nThe ≡ symbol is sometimes used to represent logical equivalence. However, Julia uses ≡ as an alias for the builtin function === which cannot have methods added to it. Use == and === to test for equivalence and identity, respectively.\n\nSee also Proposition.\n\nExamples\n\njulia> @p p == ¬p\nfalse\n\njulia> @p (p → q) ∧ (p ← q) == ¬(p ⊻ q)\ntrue\n\njulia> @p (p → q) ∧ (p ← q) === ¬(p ⊻ q)\nfalse\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#PAQ.is_tautology","page":"Semantics","title":"PAQ.is_tautology","text":"is_tautology(p)\n\nReturns a boolean on whether p is a tautology.\n\nExamples\n\njulia> is_tautology(⊤)\ntrue\n\njulia> @p is_tautology(p)\nfalse\n\njulia> @p is_tautology(¬(p ∧ ¬p))\ntrue\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#PAQ.is_contradiction","page":"Semantics","title":"PAQ.is_contradiction","text":"is_contradiction(p)\n\nReturns a boolean on whether p is a contradiction.\n\nExamples\n\njulia> is_contradiction(⊥)\ntrue\n\njulia> @p is_contradiction(p)\nfalse\n\njulia> @p is_contradiction(p ∧ ¬p)\ntrue\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#PAQ.is_truth","page":"Semantics","title":"PAQ.is_truth","text":"is_truth(p)\n\nReturns a boolean on whether p is a truth value (either a tautology or contradiction).\n\nSee also Proposition.\n\nExamples\n\njulia> is_truth(⊤)\ntrue\n\njulia> @p is_truth(p ∧ ¬p)\ntrue\n\njulia> @p is_truth(p)\nfalse\n\njulia> @p is_truth(p ∧ q)\nfalse\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#PAQ.is_contingency","page":"Semantics","title":"PAQ.is_contingency","text":"is_contingency(p)\n\nReturns a boolean on whether p is a contingency (neither a tautology or contradiction).\n\nSee also Proposition.\n\nExamples\n\njulia> is_contingency(⊤)\nfalse\n\njulia> @p is_contingency(p ∧ ¬p)\nfalse\n\njulia> @p is_contingency(p)\ntrue\n\njulia> @p is_contingency(p ∧ q)\ntrue\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#PAQ.is_satisfiable","page":"Semantics","title":"PAQ.is_satisfiable","text":"is_satisfiable(p)\n\nReturns a boolean on whether p is satisfiable (not a contradiction).\n\nSee also Proposition.\n\nExamples\n\njulia> is_satisfiable(⊤)\ntrue\n\njulia> @p is_satisfiable(p ∧ ¬p)\nfalse\n\njulia> @p is_satisfiable(p)\ntrue\n\njulia> @p is_satisfiable(p ∧ q)\ntrue\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#PAQ.is_falsifiable","page":"Semantics","title":"PAQ.is_falsifiable","text":"is_falsifiable(p)\n\nReturns a boolean on whether p is falsifiable (not a tautology).\n\nSee also Proposition.\n\nExamples\n\njulia> is_falsifiable(⊥)\ntrue\n\njulia> @p is_falsifiable(¬(p ∧ ¬p))\nfalse\n\njulia> @p is_falsifiable(p)\ntrue\n\njulia> @p is_falsifiable(p ∧ q)\ntrue\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#PAQ.solve","page":"Semantics","title":"PAQ.solve","text":"solve(p)\n\nReturn a vector of every valid interpretation of p.\n\nExamples\n\njulia> @p solve(p ⊻ q)\n2-element Vector{Vector{Pair{Atom{Symbol}}}}:\n [p => ⊥, q => ⊤]\n [p => ⊤, q => ⊥]\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#PAQ.converse","page":"Semantics","title":"PAQ.converse","text":"converse(::BooleanOperator)\n\nReturns the BooleanOperator that is the converse of the given boolean operator.\n\nExamples\n\njulia> converse(and)\nand (generic function with 20 methods)\n\njulia> @p and(p, q) == converse(and)(q, p)\ntrue\n\njulia> converse(imply)\nconverse_imply (generic function with 9 methods)\n\njulia> @p imply(p, q) == converse(imply)(q, p)\ntrue\n\n\n\n\n\n","category":"function"},{"location":"manual/semantics/#PAQ.dual","page":"Semantics","title":"PAQ.dual","text":"dual(::BooleanOperator)\n\nReturns the BooleanOperator that is the dual of the given boolean operator.\n\nExamples\n\njulia> dual(and)\nor (generic function with 17 methods)\n\njulia> @p and(p, q) == not(dual(and)(not(p), not(q)))\ntrue\n\njulia> dual(imply)\nnot_converse_imply (generic function with 9 methods)\n\njulia> @p imply(p, q) == not(dual(imply)(not(p), not(q)))\ntrue\n\n\n\n\n\n","category":"function"},{"location":"manual/utility/#Utility","page":"Utility","title":"Utility","text":"","category":"section"},{"location":"manual/utility/","page":"Utility","title":"Utility","text":"@atoms\n@p\n@p_str\nget_atoms","category":"page"},{"location":"manual/utility/#PAQ.@atoms","page":"Utility","title":"PAQ.@atoms","text":"@atoms(ps...)\n\nInstantiate and define Atoms with symbols and return a vector containing them.\n\ninfo: Info\nAtoms are defined in the global scope as constants.\n\nExamples\n\njulia> @atoms p q\n2-element Vector{Atom{Symbol}}:\n p\n q\n\njulia> p\nAtom:\n p\n\njulia> q\nAtom:\n q\n\n\n\n\n\n","category":"macro"},{"location":"manual/utility/#PAQ.@p","page":"Utility","title":"PAQ.@p","text":"@p(x)\n\nReturns a propositions by instantiating all strings and undefined variables as Atoms, and then evaluating the expression.\n\nExamples\n\njulia> x = @p p\nAtom:\n p\n\njulia> @p x ∧ q → \"r\"\nTree:\n (p ∧ q) → \"r\"\n\n\n\n\n\n","category":"macro"},{"location":"manual/utility/#PAQ.@p_str","page":"Utility","title":"PAQ.@p_str","text":"@p_str(x)\n\nExamples\n\njulia> p = @p_str(\"x\")\nAtom:\n x\n\njulia> p\"\\\"p\\\" ∧ p, Clause(and)\"\n(\"p\" ∧ x, ⊤)\n\n\n\n\n\n","category":"macro"},{"location":"manual/utility/#PAQ.get_atoms","page":"Utility","title":"PAQ.get_atoms","text":"get_atoms(::Proposition...)\n\nReturns a vector of unique Atoms contained in the given Proposition(s).\n\nwarning: Warning\nSome atoms may optimized out of an expression, such as in p ∧ ⊥ == ⊥.\n\nExamples\n\njulia> @p get_atoms(p ∧ q)\n2-element Vector{Atom{Symbol}}:\n p\n q\n\n\n\n\n\n","category":"function"},{"location":"manual/printing/#Pretty-Printing","page":"Printing","title":"Pretty Printing","text":"","category":"section"},{"location":"manual/printing/","page":"Printing","title":"Printing","text":"print_tree\ntruth_table\n@truth_table","category":"page"},{"location":"manual/printing/#AbstractTrees.print_tree","page":"Printing","title":"AbstractTrees.print_tree","text":"print_tree(p, max_depth = typemax(Int64))\n\nPrints a tree diagram of p.\n\nIf p isn't a Tree, it will be converted to one. The optional argument max_depth will truncate sub-trees at that depth.\n\njulia> @p print_tree(p ⊻ q)\n⊻\n├─ p\n└─ q\n\njulia> @p print_tree((p ∧ ¬q) ∨ (¬p ∧ q))\n∨\n├─ ∧\n│  ├─ p\n│  └─ ¬\n│     └─ q\n└─ ∧\n   ├─ ¬\n   │  └─ p\n   └─ q\n\n\n\n\n\n","category":"function"},{"location":"manual/printing/#PAQ.truth_table","page":"Printing","title":"PAQ.truth_table","text":"truth_table(::AbstractArray; numbered_rows = false)\ntruth_table(ps...; numbered_rows = false)\n\nPrint a truth table for the given Propositions and BinaryOperators.\n\nIf numbered_rows = true, the first column will contain each row's sequential number.\n\nThe first row of the header is the expression representing that column's proposition, while the second row indicates that expression's type. Logically equivalent propositions will be grouped in the same column, seperated by a comma.\n\nSee also tautology and contradiction.\n\nExamples\n\njulia> @p truth_table(p ∧ ¬p, p ∧ q)\n┌────────┬──────┬──────┬───────┐\n│ p ∧ ¬p │ p    │ q    │ p ∧ q │\n│ Tree   │ Atom │ Atom │ Tree  │\n├────────┼──────┼──────┼───────┤\n│ ⊥      │ ⊤    │ ⊤    │ ⊤     │\n│ ⊥      │ ⊥    │ ⊤    │ ⊥     │\n├────────┼──────┼──────┼───────┤\n│ ⊥      │ ⊤    │ ⊥    │ ⊥     │\n│ ⊥      │ ⊥    │ ⊥    │ ⊥     │\n└────────┴──────┴──────┴───────┘\n\njulia> truth_table([⊻, imply], numbered_rows = true)\n┌───┬──────┬──────┬────────┬────────┐\n│ # │ _    │ __   │ _ ⊻ __ │ _ → __ │\n│   │ Atom │ Atom │ Tree   │ Tree   │\n├───┼──────┼──────┼────────┼────────┤\n│ 1 │ ⊤    │ ⊤    │ ⊥      │ ⊤      │\n│ 2 │ ⊥    │ ⊤    │ ⊤      │ ⊤      │\n├───┼──────┼──────┼────────┼────────┤\n│ 3 │ ⊤    │ ⊥    │ ⊤      │ ⊥      │\n│ 4 │ ⊥    │ ⊥    │ ⊥      │ ⊤      │\n└───┴──────┴──────┴────────┴────────┘\n\n\n\n\n\n","category":"function"},{"location":"manual/printing/#PAQ.@truth_table","page":"Printing","title":"PAQ.@truth_table","text":"@truth_table(ps...; numbered_rows = false)\n\nEquivalent to @p truth_table(ps...; numbered_rows = false).\n\nSee also @p and truth_table.\n\nExamples\n\njulia> @truth_table ¬p Clause(and, p, q)\n┌──────┬──────┬─────────┬────────┐\n│ p    │ q    │ ¬p      │ p ∧ q  │\n│ Atom │ Atom │ Literal │ Clause │\n├──────┼──────┼─────────┼────────┤\n│ ⊤    │ ⊤    │ ⊥       │ ⊤      │\n│ ⊥    │ ⊤    │ ⊤       │ ⊥      │\n├──────┼──────┼─────────┼────────┤\n│ ⊤    │ ⊥    │ ⊥       │ ⊥      │\n│ ⊥    │ ⊥    │ ⊤       │ ⊥      │\n└──────┴──────┴─────────┴────────┘\n\njulia> @truth_table (⊻) imply numbered_rows = true\n┌───┬──────┬──────┬────────┬────────┐\n│ # │ _    │ __   │ _ ⊻ __ │ _ → __ │\n│   │ Atom │ Atom │ Tree   │ Tree   │\n├───┼──────┼──────┼────────┼────────┤\n│ 1 │ ⊤    │ ⊤    │ ⊥      │ ⊤      │\n│ 2 │ ⊥    │ ⊤    │ ⊤      │ ⊤      │\n├───┼──────┼──────┼────────┼────────┤\n│ 3 │ ⊤    │ ⊥    │ ⊤      │ ⊥      │\n│ 4 │ ⊥    │ ⊥    │ ⊥      │ ⊤      │\n└───┴──────┴──────┴────────┴────────┘\n\n\n\n\n\n","category":"macro"},{"location":"#Home","page":"Home","title":"Home","text":"","category":"section"},{"location":"#Introduction","page":"Home","title":"Introduction","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"If you like propositional logic, then you've come to the right place!","category":"page"},{"location":"","page":"Home","title":"Home","text":"P∧Q has an intuitive interface that enables you to manipulate logical expressions symbolically. Propositions have multiple representations which can be easily converted and extended. Several utilities have been provided for convenience, visualization, and solving propositions.","category":"page"},{"location":"#Showcase","page":"Home","title":"Showcase","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"julia> import Pkg\n\njulia> Pkg.add(url = \"https://github.com/jakobjpeters/PAQ.jl\")\n\njulia> using PAQ\n\njulia> ¬⊥\ntautology (generic function with 1 method)\n\njulia> @atoms p q\n2-element Vector{Atom{Symbol}}:\n p\n q\n\njulia> r = ¬p\nLiteral:\n ¬p\n\njulia> s = @p q ∧ r\nTree:\n q ∧ ¬p\n\njulia> interpret(s, p => ⊥)\nAtom:\n q\n\njulia> Valuation(s)\nValuation:\n [q => ⊤, p => ⊤] => ⊥\n [q => ⊥, p => ⊤] => ⊥\n [q => ⊤, p => ⊥] => ⊤\n [q => ⊥, p => ⊥] => ⊥\n\njulia> @p Clause(and, r, t, ¬u)\nClause:\n ¬p ∧ t ∧ ¬u\n\njulia> @truth_table p ∧ ¬p r p ⊻ q (p ∨ q) ∧ (p ⊼ q)\n┌────────┬──────┬──────┬─────────┬──────────────────────────┐\n│ p ∧ ¬p │ p    │ q    │ ¬p      │ p ⊻ q, (p ∨ q) ∧ (p ⊼ q) │\n│ Tree   │ Atom │ Atom │ Literal │ Tree, Tree               │\n├────────┼──────┼──────┼─────────┼──────────────────────────┤\n│ ⊥      │ ⊤    │ ⊤    │ ⊥       │ ⊥                        │\n│ ⊥      │ ⊥    │ ⊤    │ ⊤       │ ⊤                        │\n├────────┼──────┼──────┼─────────┼──────────────────────────┤\n│ ⊥      │ ⊤    │ ⊥    │ ⊥       │ ⊤                        │\n│ ⊥      │ ⊥    │ ⊥    │ ⊤       │ ⊥                        │\n└────────┴──────┴──────┴─────────┴──────────────────────────┘","category":"page"},{"location":"#Related-Packages","page":"Home","title":"Related Packages","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Julog.jl\nLogicCircuits.jl\nSymbolics.jl\nRewrite.jl\nSimplify.jl\nMetatheory.jl\nTruthTables.jl","category":"page"},{"location":"internals/#Internals","page":"Internals","title":"Internals","text":"","category":"section"},{"location":"internals/#Operators","page":"Internals","title":"Operators","text":"","category":"section"},{"location":"internals/","page":"Internals","title":"Internals","text":"PAQ.NullaryOperator\nPAQ.UnaryOperator\nPAQ.BinaryOperator\nPAQ.BooleanOperator\nPAQ.CommutativeOperator\nPAQ.AssociativeOperator\nPAQ.LeftIdentityOperator\nPAQ.RightIdentityOperator\nPAQ.AndOr","category":"page"},{"location":"internals/#PAQ.NullaryOperator","page":"Internals","title":"PAQ.NullaryOperator","text":"NullaryOperator\n\nThe union types of BooleanOperators that take zero arguments.\n\n\n\n\n\n","category":"type"},{"location":"internals/#PAQ.UnaryOperator","page":"Internals","title":"PAQ.UnaryOperator","text":"UnaryOperator\n\nThe union types of BooleanOperators that take one argument.\n\n\n\n\n\n","category":"type"},{"location":"internals/#PAQ.BinaryOperator","page":"Internals","title":"PAQ.BinaryOperator","text":"BinaryOperator\n\nThe union types of BooleanOperators that take two arguments.\n\n\n\n\n\n","category":"type"},{"location":"internals/#PAQ.BooleanOperator","page":"Internals","title":"PAQ.BooleanOperator","text":"BooleanOperator\n\nThe union types of Boolean Operators.\n\n\n\n\n\n","category":"type"},{"location":"internals/#PAQ.CommutativeOperator","page":"Internals","title":"PAQ.CommutativeOperator","text":"CommutativeOperator\n\nThe union types of BooleanOperators with the commutative property.\n\n\n\n\n\n","category":"type"},{"location":"internals/#PAQ.AssociativeOperator","page":"Internals","title":"PAQ.AssociativeOperator","text":"AssociativeOperator\n\nThe union types of BooleanOperators with the associative property.\n\n\n\n\n\n","category":"type"},{"location":"internals/#PAQ.LeftIdentityOperator","page":"Internals","title":"PAQ.LeftIdentityOperator","text":"LeftIdentityOperator\n\nThe union types of BooleanOperators that have a left identity.\n\n\n\n\n\n","category":"type"},{"location":"internals/#PAQ.RightIdentityOperator","page":"Internals","title":"PAQ.RightIdentityOperator","text":"RightIdentityOperator\n\nThe union types of BooleanOperators that have a right identity.\n\n\n\n\n\n","category":"type"},{"location":"internals/#PAQ.AndOr","page":"Internals","title":"PAQ.AndOr","text":"AndOr\n\nThe union types of and and or.\n\n\n\n\n\n","category":"type"},{"location":"internals/#Propositions","page":"Internals","title":"Propositions","text":"","category":"section"},{"location":"internals/","page":"Internals","title":"Internals","text":"PAQ.AtomicProposition\nPAQ.LiteralProposition\nPAQ.NonExpressive","category":"page"},{"location":"internals/#PAQ.AtomicProposition","page":"Internals","title":"PAQ.AtomicProposition","text":"AtomicProposition\n\nA Proposition that is known by its type to be equivalent to an Atom.\n\n\n\n\n\n","category":"type"},{"location":"internals/#PAQ.LiteralProposition","page":"Internals","title":"PAQ.LiteralProposition","text":"LiteralProposition\n\nA Proposition that is known by its type to be equivalent to a Literal.\n\n\n\n\n\n","category":"type"},{"location":"internals/#PAQ.NonExpressive","page":"Internals","title":"PAQ.NonExpressive","text":"NonExpressive\n\nA Proposition that is not an Expressive.\n\n\n\n\n\n","category":"type"},{"location":"internals/#Printing","page":"Internals","title":"Printing","text":"","category":"section"},{"location":"internals/","page":"Internals","title":"Internals","text":"show\nprint","category":"page"},{"location":"internals/#Base.show","page":"Internals","title":"Base.show","text":"show\n\n\n\n\n\n","category":"function"},{"location":"internals/#Base.print","page":"Internals","title":"Base.print","text":"print\n\n\n\n\n\n","category":"function"},{"location":"internals/#Utility","page":"Internals","title":"Utility","text":"","category":"section"},{"location":"internals/","page":"Internals","title":"Internals","text":"mapfoldl\nmapfoldr\nmapreduce","category":"page"},{"location":"internals/#Base.mapfoldl","page":"Internals","title":"Base.mapfoldl","text":"mapfoldl\n\n\n\n\n\n","category":"function"},{"location":"internals/#Base.mapfoldr","page":"Internals","title":"Base.mapfoldr","text":"mapfoldr\n\n\n\n\n\n","category":"function"},{"location":"internals/#Base.mapreduce","page":"Internals","title":"Base.mapreduce","text":"mapreduce\n\n\n\n\n\n","category":"function"},{"location":"internals/#Semantics","page":"Internals","title":"Semantics","text":"","category":"section"},{"location":"internals/","page":"Internals","title":"Internals","text":"promote_rule\nconvert","category":"page"},{"location":"internals/#Base.promote_rule","page":"Internals","title":"Base.promote_rule","text":"promote_rule\n\n\n\n\n\n","category":"function"},{"location":"internals/#Base.convert","page":"Internals","title":"Base.convert","text":"convert\n\n\n\n\n\n","category":"function"},{"location":"tutorial/#Tutorial","page":"Tutorial","title":"Tutorial","text":"","category":"section"}]
}
