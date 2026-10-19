# YAVSRG code style guidelines

### Commit messages

We mostly follow [semantic commit messages](https://gist.github.com/joshbuchea/6f47e86d2510bce28f8e7f42ae84c716)  
e.g. `feat: added a new button in ...`, `fix: crash when ...`

But since PRs are squashed when merging, you do not need to be strict with this!  
Use any convention you like, the PR title will be what ends up in `main` and your commits will form the description of that squash commit

### Code style

The vast majority of nitpicky things like indentation, spacing, use of brackets are fully automated, here is a list of things that are not:

Prefer `snake_case` variable names over `camelCase`  
Reason: I have personal preference for it, new variables should do this to match the existing codebase

Use type annotations on all properties, arguments and return types for new code (unless it's a small one-liner)  
Bad: `let calculate(rate, chart) = ...`  
Good `let calculate(rate: Rate, chart: Chart) : Difficulty = ...`  
Reason: Aids in reading the code when viewing it somewhere that won't label the types automatically, and catches otherwise quietly changing inferred types

Do not use currying in methods top-level module functions for new code  
Bad: `let calculate (rate: Rate) (chart: Chart) : Difficulty = ...`  
Good: `let calculate(rate: Rate, chart: Chart) : Difficulty = ...`  
Reason: It's not really clear which one to use over the other, so I'm ruling to use as little currying as necessary for new code

Avoid using the `<|` operator  
Bad: `function <| input`  
Good: `function(input)`  
Reason: It adds another way to write things, again making everything consistent is better

For all other matters of taste, running `./scripts/yavsrg.sh format` immediately applies mine so you don't have to worry about it  
Don't forget to run this before submitting PRs!