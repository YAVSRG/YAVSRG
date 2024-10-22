# YAVSRG code style guidelines

### Code style, PRs and other such things
- If unsure what to name your branch, I would leave it as 'main' (but I'm open to suggestions)
- At the moment I review all PRs, with an eye to help you understand what changes need to be made if any
- Prefer `snake_case` variable names over `camelCase`
- There will soon be a script to run automatic indentation/formatting on all code - hence don't think to deeply about this kind of thing and just submit the PR :)  

### Commit messages

I like to put emoji in commit messages  
You don't have to care about them - in PRs I can just squash with an emoji commit message

👽️ = Bug fix
🌸 = UI improvement
✨ = New user-facing feature
🔥 = New developer-facing feature/tooling
🏗️ = Stuff that is WIP/hidden from users
🧹 = Code refactoring/cleanup/formatting
💡 = Automated tests
⚡ = CI pipelines
📘 = Documentation for users
📕 = Documentation for developers
🎓 = Localisation changes
🏷️ = Releases
💚 = Community content

### Development principles

**Relatively speaking, the developer experience doesn't matter**  
Users don't care how pretty the codebase is as long as the game works, contributors who are "users" of the codebase are far fewer than players who are "users" of the compiled output.  
The YAVSRG codebase is kept in a state that is good enough for me and that I'm happy maintaining, but the code isn't gonna be ultra clean/perfectly engineered/optimised for reading.
If more people become regular contributors then more accomodations will be made for them.

**You choose, you lose (AKA less is more)**  
Too much choice leads to a worse experience. Users often pick what I think is the wrong option for them, or stick to what is familiar.
One-size-fits-all systems are simpler to test, simpler to maintain and simpler to use and I prefer to lean towards this when designing new features.
Only make something optional if there are existing users that want it off and you agree with and understand why they want it that way.