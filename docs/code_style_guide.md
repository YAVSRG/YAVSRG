# YAVSRG code style guidelines

### Branch naming

I would just name them 'main' to match the central repo

### Commit messages

I like to put emoji in commit messages  
You don't have to care about them in PRs I can just squash with an emoji commit message

👽️ = Bug fix
🌸 = UI improvement
✨ = New user-facing feature
🧱 = New codebase feature that doesn't (yet) affect users
📑 = Code rearrangement/cleanup/formatting
⚡️ = Automation (CI pipelines, automated tests, developer tooling)
📘 = Documentation for users
📕 = Documentation for developer workflows
🎓 = Localisation changes
🏷️ = Releases
💚 = Community content & Automated commits

### Code formatting/style

Try to follow the existing format style, but there will soon be a script to run Fantomas formatting on all code so don't worry and just submit the PR :)  

Prefer `snake_case` variable names over `camelCase`