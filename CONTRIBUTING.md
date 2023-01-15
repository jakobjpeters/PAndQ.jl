
# Contributing

When interacting with other people, please [*mind your ```ps ∧ qs```*](https://en.wikipedia.org/wiki/Mind_your_Ps_and_Qs).


## Issues


## Pull Requests


## Style Guide

- Names should be descriptive and not abbreviated.
    - If a name is too long or complex, assess whether refactoring would be useful.
- Names of collections should be pluralized.
    - Use the singular form when iterating over collections.
        - For example: ```for x in xs```.
- Use symbols as an alias for names when appropriate.
    - For example:
        ```jldoctest
        or = ¬(¬p ∧ ¬q)
        const ∨ = or
        ```
- Names should be as generic as what they represent.
- Use [```snake_case```](https://en.wikipedia.org/wiki/Snake_case) when naming variables and functions.

- Files begin and end with an empty line.
- Helper functions that are only useful to a single function or set of methods should be:
    - Named with an underscore followed by the original function's name.
    - Placed directly below the function/set of methods.

    - If there are multiple helper functions, try to use dispatch rules to distinguish between them.
        - If further distinction is needed, assess whether refactoring would be useful.
            - Otherwise, use descriptive names.
    - If a helper function is complex, consider writing its own documentation.
- Use a functional programming style as much as possible;
    - Except when needed for performance optimizations.
- Use 4 spaces instead of a tab.
- Use symbols for infix expressions and when they are standalone meaningful.
    `⊥` is meainingful by iteself, but `∧` is not.
    Use names otherwise.
    - `p ∧ q`
    - `reduce(and, ps)`
    - [⊤, ⊥]
    - not(p.p)

Otherwise, follow Julia's official [style guide](https://docs.julialang.org/en/v1/manual/style-guide/).


## To-do

- Implement
    - Sets
    - First order logic
    - Second order logic
    - Higher order logic
    - Modal logic
- Finish documentation
    - Tutorial
- Write unit tests
