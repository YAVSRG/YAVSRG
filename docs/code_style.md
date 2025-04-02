# YAVSRG code style guidelines

### Commit messages

I like to put emoji in commit messages  
You don't have to care about them - in PRs I can just squash with an emoji commit message

👽️ = Bug fix  
✨ = New client feature  
🔥 = New developer feature, experiment or tool  
⚙️ = New library feature, not available directly to users  
🌍 = New server-side/online feature  
🌸 = User interface improvement (no new functionality)  
🧹 = Code refactoring/cleanup/formatting  
💡 = Automated tests and CI pipelines  
📘 = User documentation / wiki / website  
📕 = Developer documentation / wiki  
🎓 = Localisation changes  
🏷️ = Releases  
💚 = Community content  

### Pull requests

Step 1: Create a fork of YAVSRG  

Step 2: Make your changes  
I reccommend doing this in a branch other than `main` as it makes it easy to have both your fork and the upstream as a remote

Step 3: [Submit PR](https://github.com/YAVSRG/YAVSRG/pulls)

Pull requests will be reviewed by Percyqaz for correctness and code style, then merged if all OK

### Code style
When in doubt follow the [F# coding conventions](https://learn.microsoft.com/en-us/dotnet/fsharp/style-guide/conventions)

Common variable names
- `i` = used as a nondescript loop variable. Often as the index for a "note row" when iterating through chart data
- `k` = Short for key, used instead of `i` when iterating through columns in note data

Use other single-letter names sparingly

Prefer `snake_case` variable names over `camelCase`
```fsharp
let my_variable = 5 // :)
let myVariable = 5 // >:(
```

There will soon be a script to run automatic indentation/formatting on all code via Fantomas, which will define how indentation, spacing, etc should be  
So basically don't worry about or pay much mind to your formatting as it will be automatically corrected

Top-level functions inside modules must have type annotations on all arguments and the return type  
```fsharp
module Difficulty =

	let calculate rate chart = ... // :(
	
	let calculate (rate: Rate) (chart: Chart) = ... // :(
	
	let calculate (rate: Rate) (chart: Chart) : Difficulty = ... // :)
```
Nested functions may not need types if they are short and clear, but lean towards also adding annotations anyway

Methods on classes must have type annotations on all parameters and the return type  
`.Draw()` and `.Update(elapsed_ms, moved)` are excepted from this for the time being

Methods should never use currying and instead have a single bracketed arguments list
```fsharp
type ScoreProcessor(...) =

	member this.ProcessHit(delta, is_missed) = ... // :(
	
	member this.ProcessHit (delta: GameplayTime) (is_missed: bool) : ComboAction * GameplayAction = ... // :(

	member this.ProcessHit(delta: GameplayTime, is_missed: bool) : ComboAction * GameplayAction = ... // :)

```
Annotations are not mandatory on properties, but lean towards adding them anyway

Do not swallow exceptions, log and return an option  
Instead, the caller should receive a result where it has more context on what to log  
Also, be as specific with what exceptions to catch as possible
```fsharp
// Bad
let beatmap_from_file (path: string) : Beatmap option =
	try
		...
		Some beatmap
	with err ->
		Logging.Error(... err ...)
		None
		
let caller () =
	match beatmap_from_file "my_file.osu" with
	| Some beatmap -> ...
	| None -> Logging.Error("Log twice and without context")
		
// OK
let beatmap_from_file (path: string) : Result<Beatmap, exn> =
	try
		...
		Ok beatmap
	with
	| :? RelevantException err -> Error err
		
let caller () =
	match beatmap_from_file "my_file.osu" with
	| Ok beatmap -> ...
	| Error exn -> Logging.Error("Error while trying to do ...: ...")
```

For consistency, array subscripting should always use a `.`, even where not having it would parse due to F#'s idiotic subscript parsing rules  
This intentionally ignores [the recommendation here](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/arrays#access-elements)  
```fsharp
my_array.[i] // OK
my_array[i] // parses but not OK

my_2d_array.[i].[j] // OK
my_2d_array[i].[j] // parses but not OK
my_2d_array[i][j] // does not parse

my_string.Split().[0] // OK
my_string.Split()[0] // does not parse
```

### Development principles

**Prefer one good way to do things**

The average player normally just wants one button that will "do the thing" - They often don't care that you've come up with 5 different settings that thing could be on and will just pick what's familiar.  
It is better to create one-size-fits-all systems and features, they are simpler to maintain and test as they have fewer code paths, and improvements benefit a larger proportion of players at a time.  
Optional features will be added if:
- Several users exist that want each variant of the option
- It is easy to justify each variant's distinct usefulness